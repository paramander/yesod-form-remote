{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Yesod.Form.Remote
       ( RemoteFormResult (..)
       , rreq
       , ropt
       , runRemotePost
       ) where

import           Control.Arrow    ((***))
import           Control.Monad    (liftM)
import qualified Data.Map         as Map
import           Data.Maybe       (fromMaybe)
import           Data.Text        (Text)
import           Yesod.Core       (HandlerSite, MonadHandler, RenderMessage,
                                   SomeMessage (..), getYesod, languages,
                                   renderMessage, runRequestBody)
import           Yesod.Form.Types (Env, Field (..), FileEnv, FormMessage (..))

type ErrorMap = [(Text, Text)]

newtype FormRemote m a = FormRemote
    { unFormRemote :: HandlerSite m -> [Text] -> Env -> FileEnv -> m (Either ErrorMap a)
    }

data RemoteFormResult a = RemoteFormFailure ErrorMap
                        | RemoteFormSuccess a

instance Monad m => Functor (FormRemote m) where
    fmap a (FormRemote f) = FormRemote $ \c d e e' -> liftM (either Left (Right . a)) $ f c d e e'

instance Monad m => Applicative (FormRemote m) where
    pure = FormRemote . const . const . const . const . return . Right
    (FormRemote f) <*> (FormRemote x) = FormRemote $ \c d e e' -> do
        res1 <- f c d e e'
        res2 <- x c d e e'
        return $ case (res1, res2) of
            (Left a, Left b) -> Left $ a ++ b
            (Left a, _) -> Left a
            (_, Left b) -> Left b
            (Right a, Right b) -> Right $ a b

rreq :: (Monad m, RenderMessage (HandlerSite m) FormMessage)
     => Field m a
     -> Text
     -> FormRemote m a
rreq field name = FormRemote $ \m l env fenv -> do
    let filteredEnv = fromMaybe [] $ Map.lookup name env
        filteredFEnv = fromMaybe [] $ Map.lookup name fenv
    emx <- fieldParse field filteredEnv filteredFEnv
    return $ case emx of
        Left (SomeMessage e) -> Left $ [(name, renderMessage m l e)]
        Right Nothing -> Left $ [(name, renderMessage m l $ MsgInputNotFound name)]
        Right (Just a) -> Right a

ropt :: Monad m
     => Field m a
     -> Text
     -> FormRemote m (Maybe a)
ropt field name = FormRemote $ \m l env fenv -> do
    let filteredEnv = fromMaybe [] $ Map.lookup name env
        filteredFEnv = fromMaybe [] $ Map.lookup name fenv
    emx <- fieldParse field filteredEnv filteredFEnv
    return $ case emx of
        Left (SomeMessage e) -> Left $ [(name, renderMessage m l e)]
        Right x -> Right x

runRemotePost :: MonadHandler m
              => FormRemote m a
              -> m (RemoteFormResult a)
runRemotePost (FormRemote f) = do
    (env, fenv) <- liftM (toMap *** toMap) runRequestBody
    m <- getYesod
    l <- languages
    fmap (either RemoteFormFailure RemoteFormSuccess) $ f m l env fenv
    where toMap :: [(Text, a)]
                -> Map.Map Text [a]
          toMap = Map.unionsWith (++) . map (\(x, y) -> Map.singleton x [y])

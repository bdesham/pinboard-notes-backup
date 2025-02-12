{-# LANGUAGE CPP #-}

module Utils.HttpConfig (makeHttpConfig) where

import qualified Network.HTTP.Req as Req

#if MIN_VERSION_tls(2,0,0)
import Data.Default.Class
import Network.Connection (TLSSettings (..))
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.TLS (EMSMode (..), Supported (..))
#endif

-- | Creates an 'Network.HTTP.Req.HttpConfig' suitable for connecting to the Pinboard API server.
--
-- As of February 2025, the latest TLS version supported by the Pinboard server is 1.2, and the
-- server does not support the Extended Main Secret (also called Extended Master Secret or EMS)
-- extension. As of version 2.0.0 of the tls library, we need to use a custom configuration to
-- connect to such a server.
makeHttpConfig :: IO Req.HttpConfig

#if MIN_VERSION_tls(2,0,0)

makeHttpConfig = do
    let supported = def {supportedExtendedMainSecret=AllowEMS}
        tlsSettings = TLSSettingsSimple False False False supported
        managerSettings = mkManagerSettings tlsSettings Nothing
    manager <- newManager managerSettings
    pure $ Req.defaultHttpConfig {Req.httpConfigAltManager=Just manager}

#else

makeHttpConfig = pure Req.defaultHttpConfig

#endif

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Cook.Docker.Tls where

import Control.Lens ((&), (.~))
import Data.Default
import Data.Monoid
import Data.X509.CertificateStore
import Data.X509.File
import Network.Connection (TLSSettings(..))
import Network.HTTP.Client (ManagerSettings)
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.TLS
import Network.TLS.Extra.Cipher
import Network.Wreq
import System.Directory
import System.Environment
import System.FilePath


data UseCA
    = WithCA FilePath
    | NoCA

data UseClientCert
    = WithClientCert FilePath FilePath
    | NoClientCert


getTlsManagerSettings :: UseClientCert -> UseCA -> String -> IO ManagerSettings
getTlsManagerSettings useCC useCA host =
    do cas <-
           case useCA of
             WithCA certPath -> readSignedObject certPath
             NoCA -> return []
       creds <-
           case useCC of
             WithClientCert certPath keyPath ->
                 either error Just <$> credentialLoadX509 certPath keyPath
             NoClientCert -> return Nothing
       let hooks =
               def
               { onCertificateRequest =
                     \_ -> return creds
               , onServerCertificate =
                     \store -> onServerCertificate def
                               (store <> makeCertificateStore cas)
               }
           clientParams =
               (defaultParamsClient host "")
               { clientHooks = hooks
               , clientSupported = def { supportedCiphers = ciphersuite_strong }
               }
           tlsSettings = TLSSettings clientParams
       return $ mkManagerSettings tlsSettings Nothing

tlsDockerOpts :: String -> IO Options
tlsDockerOpts host =
    do explicitCertsPath <- lookupEnv "DOCKER_CERT_PATH"
       certsPath <-
           case explicitCertsPath of
             Just path -> return path
             Nothing ->
                 (</> ".docker") <$> getEnv "HOME"
       let certPath = certsPath </> "cert.pem"
           keyPath = certsPath </> "key.pem"
           caPath = certsPath </> "ca.pem"
       clientCertsExist <-
           (&&) <$> doesFileExist certPath <*> doesFileExist keyPath
       let useCC =
               if clientCertsExist
               then WithClientCert certPath keyPath
               else NoClientCert
           useCA = WithCA caPath
       settings <- getTlsManagerSettings useCC useCA host
       return $ defaults & manager .~ Left settings

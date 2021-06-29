-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings where

import ClassyPrelude.Yesod
import qualified Control.Exception           as Exception
import Data.Aeson                  (Result (..), fromJSON, withObject, (.!=),
                                    (.:?))
import Data.FileEmbed              (embedFile)
import Data.Yaml                   (decodeEither')
import Database.Persist.Postgresql (PostgresConf)
import Language.Haskell.TH.Syntax  (Exp, Name, Q)
import Network.Wai.Handler.Warp    (HostPreference)
import Yesod.Default.Config2       (applyEnvValue, configSettingsYml)
import Yesod.Default.Util          (WidgetFileSettings, widgetFileNoReload,
                                    widgetFileReload)

import qualified Jose.Jwk as JWK

data RepoScheme = SelfHosted | Branches
                  deriving (Eq, Show)

toRepoScheme :: Text -> RepoScheme
toRepoScheme "branches" = Branches
toRepoScheme _ = SelfHosted

data TagPermissions = OnlyAdminCanAddNewTags | EverybodyCanAddNewTags
                      deriving (Eq, Show)

toTagPermissions :: Text -> TagPermissions
toTagPermissions "everybody-can-add-new-tags" = EverybodyCanAddNewTags
toTagPermissions _ = OnlyAdminCanAddNewTags

data LeaderboardStyle = BySubmitter | ByTag
                        deriving (Eq, Show)

toLeaderboardStyle :: Text -> LeaderboardStyle
toLeaderboardStyle "by-tag" = ByTag
toLeaderboardStyle _ = BySubmitter

-- How showing progress for asynchronous operations
-- such as creating a challenge, submitting a submission, etc.
-- is realized technically.
data ViewingProgressStyle = WithWebSockets | WithPlainText
                            deriving (Eq, Show)

toViewingProgressStyle :: Text -> ViewingProgressStyle
toViewingProgressStyle "with-web-sockets" = WithWebSockets
toViewingProgressStyle _ = WithPlainText


-- | Runtime settings to configure this application. These settings can be
-- loaded from various sources: defaults, environment variables, config files,
-- theoretically even a database.
data AppSettings = AppSettings
    { appStaticDir              :: String
    -- ^ Directory from which to serve static files.
    , appDatabaseConf           :: PostgresConf
    -- ^ Configuration settings for accessing the database.
    , appRoot                   :: Text
    -- ^ Base for all generated URLs.
    , appHost                   :: HostPreference
    -- ^ Host/interface the server should bind to.
    , appPort                   :: Int
    -- ^ Port to listen on
    , appIpFromHeader           :: Bool
    -- ^ Get the IP address from the header when logging. Useful when sitting
    -- behind a reverse proxy.

    , appDetailedRequestLogging :: Bool
    -- ^ Use detailed request logging system
    , appShouldLogAll           :: Bool
    -- ^ Should all log messages be displayed?
    , appReloadTemplates        :: Bool
    -- ^ Use the reload version of templates
    , appMutableStatic          :: Bool
    -- ^ Assume that files in the static dir may change after compilation
    , appSkipCombining          :: Bool
    -- ^ Perform no stylesheet/script combining

    -- Example app-specific configuration values.
    , appCopyright              :: Text
    -- ^ Copyright text to appear in the footer of the page
    , appAnalytics              :: Maybe Text
    -- ^ Google Analytics code
    , appVarDir                 :: String
    -- ^ Contact (admin) e-mail
    , appContactEmail           :: Maybe Text
    -- ^ Ident of an admin to be created when starting
    , appAdminUser              :: Maybe Text
    -- ^ Password for an admin to be created when starting
    , appAdminPassword          :: Maybe Text
    -- ^ Additional info for the instance
    , appLocation               :: Maybe Text
    -- ^ Repo host
    , appRepoHost               :: Text
    , appRepoScheme             :: RepoScheme
    , appTagPermissions         :: TagPermissions
    , appAutoOpening            :: Bool
    , appLeaderboardStyle       :: LeaderboardStyle
    , appNewBestResultSlackHook :: Maybe Text
    , appServerSSHPublicKey     :: Maybe Text
    -- ^ Are challenges, submission, etc. visible without logging in
    , appIsPublic :: Bool
    , appJSONWebKey :: Maybe JWK.Jwk
    , appViewingProgressStyle   :: ViewingProgressStyle
    -- ^ Take the team name from a given metadata field
    -- Currently makes sense only when JWT token is used
    , appTeamField :: Maybe Text
    -- ^ Automatically assign the team.
    -- The team for which the user is the captain
    -- will be preferred
    , appAutoTeam :: Bool
    }

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        let defaultDev =
#if DEVELOPMENT
                True
#else
                False
#endif
        appStaticDir              <- o .: "static-dir"
        appDatabaseConf           <- o .: "database"
        appRoot                   <- o .: "approot"
        appHost                   <- fromString <$> o .: "host"
        appPort                   <- o .: "port"
        appIpFromHeader           <- o .: "ip-from-header"

        appDetailedRequestLogging <- o .:? "detailed-logging" .!= defaultDev
        appShouldLogAll           <- o .:? "should-log-all"   .!= defaultDev
        appReloadTemplates        <- o .:? "reload-templates" .!= defaultDev
        appMutableStatic          <- o .:? "mutable-static"   .!= defaultDev
        appSkipCombining          <- o .:? "skip-combining"   .!= defaultDev

        appCopyright              <- o .: "copyright"
        appAnalytics              <- o .:? "analytics"
        appVarDir                 <- o .: "var-dir"
        appContactEmail           <- o .:? "contact-email"

        appAdminUser              <- o .:? "admin-user"
        appAdminPassword          <- o .:? "admin-password"
        appLocation               <- o .:? "location"

        appRepoHost               <- o .: "repo-host"

        appRepoScheme             <- toRepoScheme <$> o .: "repo-scheme"
        appTagPermissions         <- toTagPermissions <$> o .: "tag-permissions"
        appAutoOpening            <- o .:? "auto-opening" .!= False
        appLeaderboardStyle       <- toLeaderboardStyle <$> o .: "leaderboard-style"

        appNewBestResultSlackHook <- o .:? "new-best-result-slack-hook"

        appServerSSHPublicKey <- o .:? "server-ssh-public-key"

        appIsPublic               <- o .:? "is-public" .!= False

        appJSONWebKey             <- o .:? "json-web-key"

        appViewingProgressStyle   <- toViewingProgressStyle <$> o .: "viewing-progress-style"

        appTeamField             <- o .:? "team-field"

        appAutoTeam              <- o .:? "auto-team" .!= False

        return AppSettings {..}

-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
--
-- For more information on modifying behavior, see:
--
-- https://github.com/yesodweb/yesod/wiki/Overriding-widgetFile
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def

-- | How static files should be combined.
combineSettings :: CombineSettings
combineSettings = def

-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: String -> Q Exp
widgetFile = (if appReloadTemplates compileTimeAppSettings
                then widgetFileReload
                else widgetFileNoReload)
              widgetFileSettings

-- | Raw bytes at compile time of @config/settings.yml@
configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml)

-- | @config/settings.yml@, parsed to a @Value@.
configSettingsYmlValue :: Value
configSettingsYmlValue = either Exception.throw id
                       $ decodeEither' configSettingsYmlBS

-- | A version of @AppSettings@ parsed at compile time from @config/settings.yml@.
compileTimeAppSettings :: AppSettings
compileTimeAppSettings =
    case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
        Error e -> error e
        Success settings -> settings

-- The following two functions can be used to combine multiple CSS or JS files
-- at compile time to decrease the number of http requests.
-- Sample usage (inside a Widget):
--
-- > $(combineStylesheets 'StaticR [style1_css, style2_css])

combineStylesheets :: Name -> [Route Static] -> Q Exp
combineStylesheets = combineStylesheets'
    (appSkipCombining compileTimeAppSettings)
    combineSettings

combineScripts :: Name -> [Route Static] -> Q Exp
combineScripts = combineScripts'
    (appSkipCombining compileTimeAppSettings)
    combineSettings


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler
where
import Found
import Model

import Develop.DatFw
import Develop.DatFw.Handler
import Develop.DatFw.Template
import Develop.DatFw.Auth
import Develop.DatFw.Form
import Develop.DatFw.Form.Fields

import           Data.Text (Text)
import           Control.Monad.IO.Class   -- imports liftIO

{---------------------------------------------------------------------
                TODO
---------------------------------------------------------------------}

themeForm :: AForm (HandlerFor Forum) Theme
themeForm =
    Theme <$> freq (checkM checkUserExists textField)
                   (withPlaceholder "Introduiu el nom de l'usuari responsable" "Nom del responsable")
                   Nothing
          <*> pure ""
          <*> freq textField (withPlaceholder "Introduiu el títol del tema" "Titol") Nothing
          <*> freq textareaField (withPlaceholder "Introduiu la descripció del tema" "Descripció") Nothing

checkUserExists :: Text -> HandlerFor Forum (Either Text Text)
checkUserExists uname = do
    users <- getsSite forumUsers
    case lookup uname users of
        Nothing -> pure $ Left "L'usuari no existeix"
        Just _  -> pure $ Right uname

getHomeR :: HandlerFor Forum Html
getHomeR = do
    -- Get model info
    db <- getsSite forumDb
    themes <- liftIO $ getThemeList db
    mbuser <- maybeAuthId
    tformw <- generateAFormPost themeForm
    -- Return HTML content
    defaultLayout $ $(widgetTemplFile "src/forum/templates/home.html")

postHomeR :: HandlerFor Forum Html
postHomeR = do
    user <- requireAuthId
    db <- getsSite forumDb
    (tformr, tformw) <- runAFormPost themeForm
    case tformr of
        FormSuccess newtheme -> do
            liftIO $ addTheme newtheme db
            redirectRoute HomeR []
        _ -> do
            themes <- liftIO $ getThemeList db
            let mbuser = Just user
            defaultLayout $(widgetTemplFile "src/forum/templates/home.html")

questionAForm :: ThemeId -> AForm (HandlerFor Forum) Question
questionAForm tid = 
	Question <$> 
		pure tid <*>
		liftToAForm requireAuthId <*>
		liftToAForm (liftIO getCurrentTime) <*>
		freq textField (withPlaceholder "Introduiu el títol del tema" "Titol") Nothing  <*>
        freq textareaField (withPlaceholder "Introduiu la descripció del tema" "Descripció") Nothing


getThemeR :: ThemeId -> HandlerFor Forum Html
getThemeR tid = do
    db <- getsSite forumDb
	case liftIO $ getTheme tid db of 
		Just theme -> do
			questions <- liftIO $ getQuestionList tid db
			mbuser <- maybeAuthId
			qformw <- generateAFormPost $ questionAFrom tid
			defaultLayout $ do 
				setTitle "Theme"
				widgetTemplFile "src/forum/templates/theme.html"
        _ -> do
            defaultLayout $ do 
				widgetTemplFile "src/forum/templates/theme.html"
	
	
    -- fail "A completar per l'estudiant"

postThemeR :: ThemeId -> HandlerFor Forum Html
postThemeR tid = do
    user <- requireAuthId
    db <- getsSite forumDb
    (qformr, qformw) <- runAFormPost $ questionAForm tid
    case qformr of 
        FormSuccess newQuestion -> do
            liftIO $ addQuestion newQuestion db
            redirectRoute ThemeR []
        _ -> do 
            questions <- liftIO $ getQuestionList tid db
            let mbuser = Just user
            let isLeader = maybe False (tLeader theme ==) mbuser
            defaultLayout $(widgetTemplFile "src/forum/templates/theme.html")

	{-isAdd <- isJust <$> lookupPostParam "add"
    isMark <- isJust <$> lookupPostParam "mark"
    isDelete <- isJust <$> lookupPostParam "delete"
	if isAdd then do
		case tformr of 
			FormSuccess newtheme -> do
				liftIO $ addTheme newtheme db
				-- todo redirect where¿
			_ -> do 
				-- ?????
	else if isMark then do 
		checkBoxes <- lookupPostParams "tid"
        let tids = catMaybes ((readMaybe . T.unpack) <$> checkBoxes)
        forM_ tids $ \ tid ->
            liftIO $ markTask tid db
        redirectRoute HomeR []

    else if isDelete then do
        checkBoxes <- lookupPostParams "tid"
        let tids = catMaybes ((readMaybe . T.unpack) <$> checkBoxes)
        forM_ tids $ \ tid ->
            liftIO $ deleteTheme tid db
        redirectRoute HomeR []
    else
        invalidArgs ["add","mark","delete"]
    -- fail "A completar per l'estudiant"-}


answerAForm :: ThemeId -> QuestionId -> AForm (HandlerFor Forum) Answer
answerAForm tid qid =
    Answer <$> pure qid
            <*> liftToAForm requireAuthId
            <*> liftToAForm $ liftIO getCurrentTime
            <*> freq textareaField (withPlaceholder "Introduiu la resposta" "Resposta") Nothing

getQuestionR :: ThemeId -> QuestionId -> HandlerFor Forum Html
getQuestionR tid qid = do   
    db <- getsSite forumDb
    theme <- do 
        maybeTheme <- liftIO $ getTheme tid db
        maybe notFound pure maybeTheme
    question <- do 
        maybeQuestio  <- liftIO $ getQuestion qid db
        maybe notFound pure maybeQuestion
    answers <- liftIO $ getAnswerList qid db
    mbuser <- maybeAuthId 
    let isLeader = maybe False (tLeader theme ==) mbuser
    aformw <- generateAFormPost (answerForm tid qid)
    -- Return HTML content
    defaultLayout $ do
        setTitle "Question"
        $(widgetTemplFile "src/forum/templates/question.html")
    --fail "A completar per l'estudiant"
    

postQuestionR :: ThemeId -> QuestionId -> HandlerFor Forum Html
postQuestionR tid qid = do
    fail "A completar per l'estudiant"


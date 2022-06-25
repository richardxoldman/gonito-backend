module Handler.Course where

import Import

import Handler.ListChallenges

import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))


data CourseInfo = CourseInfo {
   courseInfoCourse :: Course,
   courseInfoChallenges :: [ChallengeView]
}

getCourseR :: Text -> Handler Html
getCourseR courseCode = do
  courseInfo <- fetchCourseInfo courseCode

  defaultLayout $ do
    setTitle "Course"
    $(widgetFile "course")

fetchCourseInfo :: Text -> Handler CourseInfo
fetchCourseInfo courseCode = do
  (Entity courseId course) <- runDB $ getBy404 $ UniqueCourseCode courseCode

  challenges <- runDB $ E.select $ E.from $ \(challenge, course_challenge) -> do
    E.where_ (course_challenge ^. CourseChallengeCourse E.==. E.val courseId
              E.&&. course_challenge ^. CourseChallengeChallenge E.==. challenge ^. ChallengeId)
    E.orderBy [E.asc (challenge ^. ChallengeName)]
    return challenge

  chVs <- mapM fetchChallengeView $ filter (\ch -> (challengeArchived $ entityVal ch) /= Just True) challenges

  return $ CourseInfo {
    courseInfoCourse = course,
    courseInfoChallenges = chVs
  }

module Handler.MakePublic where

import Import

import Handler.Shared

import PersistSHA1

import Text.Printf
import Database.Persist.Sql

import Data.Text as T

getMakePublicR :: SubmissionId -> Handler TypedContent
getMakePublicR submissionId = runViewProgress $ doMakePublic submissionId

doMakePublic :: SubmissionId -> Channel -> Handler ()
doMakePublic submissionId chan = do
  isOwner <- checkWhetherUserRepo submissionId
  if not isOwner
   then
    err chan "Only the submitter can make a submission public!"
   else do
    msg chan "Making the submission public..."
    runDB $ update submissionId [SubmissionIsPublic =. True]
    let targetBranchName = printf "submission-%05d" $ fromSqlKey submissionId
    submission <- runDB $ get404 submissionId
    challenge <- runDB $ get404 $ submissionChallenge submission
    let submissionRepoId = submissionRepo submission
    submissionRepoDir <- getRepoDir submissionRepoId
    let targetRepoUrl = T.unpack $ gitServer ++ challengeName challenge
    msg chan $ T.pack $ "Start pushing from " ++ submissionRepoDir ++ " to repo " ++ targetRepoUrl ++ ", branch " ++ targetBranchName ++ " ..."
    let commit = submissionCommit submission
    pushRepo submissionRepoDir commit targetRepoUrl targetBranchName chan
  return ()


pushRepo :: String -> SHA1 -> String -> String -> Channel -> Handler ()
pushRepo repoDir commit targetRepoUrl targetBranchName chan = do
  (exitCode, _) <- runProgram (Just repoDir) gitPath [
    "push",
    targetRepoUrl,
    (T.unpack $ fromSHA1ToText commit) ++ ":refs/heads/" ++ targetBranchName] chan
  return ()

checkWhetherUserRepo :: SubmissionId -> Handler Bool
checkWhetherUserRepo submissionId = do
  submission <- runDB $ get404 submissionId
  userId <- requireAuthId
  return $ userId == submissionSubmitter submission

/*
 * Copyright (c) 2018, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: BSD-3-Clause
 * For full license text, see the LICENSE.txt file in the repo root or https://opensource.org/licenses/BSD-3-Clause
 */

package utils

import java.time.LocalDateTime

import models.ClaSignature
import modules.{Database, DatabaseMock}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import org.scalatestplus.play.PlaySpec
import pdi.jwt.{JwtClaim, JwtJson}
import play.api.Mode
import play.api.i18n.MessagesApi
import play.api.inject._
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json._
import play.api.libs.ws.WSClient
import play.api.test.Helpers._
import utils.GitHub._

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Random, Success, Try}


class GitHubSpec extends PlaySpec with GuiceOneAppPerSuite {

  override implicit lazy val app = new GuiceApplicationBuilder()
    .overrides(bind[Database].to[DatabaseMock])
    .configure(
      Map(
        "play.modules.disabled" -> Seq("org.flywaydb.play.PlayModule", "modules.DatabaseModule")
      )
    )
    .in(Mode.Test)
    .build()

  lazy val wsClient = app.injector.instanceOf[WSClient]

  lazy val messagesApi = app.injector.instanceOf[MessagesApi]

  lazy val gitHub = new GitHub(app.configuration, wsClient, messagesApi)(ExecutionContext.global)

  val statusUrlF = (_: String, _: String, _: Int) => "http://asdf.com"

  val testToken1 = sys.env("GITHUB_TEST_TOKEN1")
  val testToken2 = sys.env("GITHUB_TEST_TOKEN2")
  val testOrg = sys.env("GITHUB_TEST_ORG")

  lazy val testLogin1 = (await(gitHub.userInfo(testToken1)) \ "login").as[String]
  lazy val testLogin2 = (await(gitHub.userInfo(testToken2)) \ "login").as[String]

  lazy val testIntegrationInstallationId: Int = {
    val integrationInstallations = await(gitHub.integrationInstallations())

    integrationInstallations.value.find { json =>
      (json \ "account" \ "login").asOpt[String].contains(testLogin1)
    }.flatMap { json =>
      (json \ "id").asOpt[Int]
    }.getOrElse(throw new IllegalStateException(s"$testLogin1 must have the integration ${gitHub.integrationId} installed"))
  }

  lazy val testIntegrationInstallationIdOrg: Int = {
    val integrationInstallations = await(gitHub.integrationInstallations())

    integrationInstallations.value.find { json =>
      (json \ "account" \ "login").asOpt[String].contains(testOrg)
    }.flatMap { json =>
      (json \ "id").asOpt[Int]
    }.getOrElse(throw new IllegalStateException(s"$testOrg must have the integration ${gitHub.integrationId} installed"))
  }

  lazy val testIntegrationToken = (await(gitHub.installationAccessTokens(testIntegrationInstallationId)) \ "token").as[String]
  lazy val testIntegrationTokenOrg = (await(gitHub.installationAccessTokens(testIntegrationInstallationIdOrg)) \ "token").as[String]

  val maxTries = 10

  // Poll until the repo has commits - So much gross
  @tailrec
  private def waitForRepo(ownerRepo: String, token: String, tryNum: Int = 0) {
    val repo = Try(await(gitHub.repo(ownerRepo)(token)))

    if (repo.isFailure && (tryNum < maxTries)) {
      Thread.sleep(1000)
      waitForRepo(ownerRepo, token, tryNum + 1)
    }
    else if (tryNum >= maxTries) {
      throw new Exception("max tries reached")
    }
  }

  @tailrec
  private def waitForCommits(ownerRepo: String, token: String, tryNum: Int = 0) {
    val repoCommits = Try(await(gitHub.repoCommits(ownerRepo, token))).getOrElse(JsArray())

    if (repoCommits.value.isEmpty && tryNum < maxTries) {
      Thread.sleep(1000)
      waitForCommits(ownerRepo, token, tryNum + 1)
    }
    else if (tryNum >= maxTries) {
      throw new Exception("max tries reached")
    }
  }

  @tailrec
  private def waitForCommit(ownerRepo: String, sha: String, token: String, tryNum: Int = 0) {
    val repoCommit = await(gitHub.repoCommits(ownerRepo, token)).value.find(_.\("sha").as[String] == sha)

    if (repoCommit.isEmpty && (tryNum < maxTries)) {
      Thread.sleep(1000)
      waitForCommit(ownerRepo, sha, token, tryNum + 1)
    }
    else if (tryNum >= maxTries) {
      throw new Exception("max tries reached")
    }
  }

  @tailrec
  private def waitForFileToBeReady(ownerRepo: String, path: String, ref: String, accessToken: String, tryNum: Int = 0): Unit = {
    val file = Try(await(gitHub.getFile(ownerRepo, path, Some(ref))(accessToken)))

    if (file.isFailure && (tryNum < maxTries)) {
      Thread.sleep(1000)
      waitForFileToBeReady(ownerRepo, path, ref, accessToken, tryNum + 1)
    }
    else if (tryNum >= maxTries) {
      throw new Exception("max tries reached")
    }
  }

  @tailrec
  private def waitForPullRequest(ownerRepo: String, prNumber: Int, accessToken: String, tryNum: Int = 0): Unit = {

    val pr = Try(await(gitHub.getPullRequest(ownerRepo, prNumber, accessToken)))

    if (pr.isFailure && (tryNum < maxTries)) {
      Thread.sleep(1000)
      waitForPullRequest(ownerRepo, prNumber, accessToken, tryNum + 1)
    }
    else if (tryNum >= maxTries) {
      throw new Exception("max tries reached")
    }
  }

  @tailrec
  private def waitForCommitState(ownerRepo: String, sha: String, state: String, accessToken: String, tryNum: Int = 0): Unit = {
    val status = Try(await(gitHub.commitStatus(ownerRepo, sha, accessToken))).getOrElse(Json.obj())

    if (!(status \ "state").asOpt[String].contains(state) && (tryNum < maxTries)) {
      Thread.sleep(1000)
      waitForCommitState(ownerRepo, sha, state, accessToken, tryNum + 1)
    }
    else if (tryNum >= maxTries) {
      throw new Exception("max tries reached")
    }
  }

  @tailrec
  private def waitForPullRequestToHaveSha(ownerRepo: String, prNumber: Int, sha: String, accessToken: String, tryNum: Int = 0): Unit = {
    val pullRequest = Try(await(gitHub.getPullRequest(ownerRepo, prNumber, accessToken))).getOrElse(Json.obj())

    val prSha = (pullRequest \ "head" \ "sha").as[String]

    if (prSha != sha && (tryNum < maxTries)) {
      Thread.sleep(1000)
      waitForPullRequestToHaveSha(ownerRepo, prNumber, sha, accessToken, tryNum + 1)
    }
    else if (tryNum >= maxTries) {
      throw new Exception("max tries reached")
    }
  }

  private def createRepo(): String = {
    val repoName = Random.alphanumeric.take(8).mkString
    val createRepoResult = await(gitHub.createRepo(repoName, None, true)(testToken1))
    val ownerRepo = (createRepoResult \ "full_name").as[String]

    waitForCommits(ownerRepo, testToken1)
    waitForCommits(ownerRepo, testToken2)

    ownerRepo
  }

  private def createOrgRepo(org: String): String = {
    val repoName = Random.alphanumeric.take(8).mkString
    val createRepoResult = await(gitHub.createRepo(repoName, Some(org), true)(testToken1))
    val ownerRepo = (createRepoResult \ "full_name").as[String]

    waitForCommits(ownerRepo, testToken1)
    waitForCommits(ownerRepo, testToken2)

    // add an commit with a non-github committer
    val commit = await(gitHub.repoCommits(ownerRepo, testToken1)).value.head

    val sha = (commit \ "sha").as[String]
    val tree = (commit \ "commit" \ "tree" \ "sha").as[String]

    val newCommit = await(gitHub.commit(ownerRepo, "test non-github commit", tree, Set(sha), Some(repoName, s"$repoName@$repoName.com"), testToken1))

    val newSha = (newCommit \ "sha").as[String]

    val updateRef = await(gitHub.updateGitRef(ownerRepo, newSha, "heads/master", testToken1))

    waitForCommit(ownerRepo, newSha, testToken1)

    ownerRepo
  }

  private def createFork(): String = {
    val forkResult = await(gitHub.forkRepo(testRepo1)(testToken2))
    val forkOwnerRepo = (forkResult \ "full_name").as[String]

    waitForCommits(forkOwnerRepo, testToken1)
    waitForCommits(forkOwnerRepo, testToken2)

    forkOwnerRepo
  }

  def pullRequestInfo(pullRequest: JsObject): (String, Int) = {
    val ownerRepo = (pullRequest \ "pull_request" \ "base" \ "repo" \ "full_name").as[String]
    val pullRequestNum = (pullRequest \ "pull_request" \ "number").as[Int]
    (ownerRepo, pullRequestNum)
  }

  lazy val testRepo1 = createRepo()
  lazy val testRepo2 = createRepo()
  lazy val testRepo3 = createRepo()
  lazy val testFork = createFork()
  lazy val testOrgRepo = createOrgRepo(testOrg)

  def createTestExternalPullRequest() = {
    val testRepos2 = await(gitHub.userRepos(testLogin2, testToken2))

    // make sure testToken2 does not have access to the upstream repo
    testRepos2.value.find(_.\("full_name").asOpt[String].contains(testRepo1)) must be (None)

    val readme = await(gitHub.getFile(testFork, "README.md")(testToken2))

    val maybeReadmeSha = (readme \ "sha").asOpt[String]

    maybeReadmeSha must be ('defined)

    val readmeSha = maybeReadmeSha.get

    val commits = await(gitHub.repoCommits(testFork, testToken1))

    val sha = (commits.value.head \ "sha").as[String]

    waitForFileToBeReady(testFork, "README.md", sha, testToken1)
    waitForFileToBeReady(testFork, "README.md", sha, testToken2)

    val newContents = Random.alphanumeric.take(32).mkString

    // external pull request
    val editResult = await(gitHub.editFile(testFork, "README.md", newContents, "Updated", readmeSha)(testToken2))
    (editResult \ "commit").asOpt[JsObject] must be ('defined)
    val editSha = (editResult \ "commit" \ "sha").as[String]
    waitForCommit(testFork, editSha, testToken1)
    waitForCommit(testFork, editSha, testToken2)

    // testToken2 create PR to testRepo1
    val externalPullRequest = await(gitHub.createPullRequest(testRepo1, "Updates", s"$testLogin2:master", "master", testToken2))
    (externalPullRequest \ "id").asOpt[Int] must be ('defined)
    val prNumber = (externalPullRequest \ "number").as[Int]
    waitForPullRequest(testRepo1, prNumber, testToken1)
    waitForPullRequest(testRepo1, prNumber, testToken2)

    externalPullRequest
  }

  lazy val testExternalPullRequest = Json.obj("pull_request" -> createTestExternalPullRequest())


  def createTestInternalPullRequest() = {
    val newContents = Random.alphanumeric.take(32).mkString
    val newBranchName = Random.alphanumeric.take(8).mkString

    val readmeSha1 = (await(gitHub.getFile(testRepo1, "README.md")(testToken1)) \ "sha").as[String]

    val commits = await(gitHub.repoCommits(testRepo1, testToken1))

    val sha = (commits.value.head \ "sha").as[String]

    waitForFileToBeReady(testRepo1, "README.md", sha, testToken1)
    waitForFileToBeReady(testRepo1, "README.md", sha, testToken2)

    val newBranch = await(gitHub.createBranch(testRepo1, newBranchName, sha, testToken1))

    waitForFileToBeReady(testRepo1, "README.md", newBranchName, testToken1)
    waitForFileToBeReady(testRepo1, "README.md", newBranchName, testToken2)

    val internalEditResult1 = await(gitHub.editFile(testRepo1, "README.md", newContents, "Updated", readmeSha1, Some(newBranchName))(testToken1))
    (internalEditResult1 \ "commit").asOpt[JsObject] must be ('defined)
    val editSha1 = (internalEditResult1 \ "commit" \ "sha").as[String]

    waitForCommitState(testRepo1, editSha1, "pending", testToken1)

    val readmeSha2 = (await(gitHub.getFile(testRepo1, "README.md", Some(newBranchName))(testToken1)) \ "sha").as[String]

    val internalPullRequest = await(gitHub.createPullRequest(testRepo1, "Updates", newBranchName, "master", testToken1))
    (internalPullRequest \ "id").asOpt[Int] must be ('defined)
    val prNumber = (internalPullRequest \ "number").as[Int]

    waitForPullRequest(testRepo1, prNumber, testToken1)
    waitForPullRequest(testRepo1, prNumber, testToken2)

    val internalEditResult2 = await(gitHub.editFile(testRepo1, "README.md", "updated by bot", "updated by bot", readmeSha2, Some(newBranchName))(testIntegrationToken))
    val editSha2 = (internalEditResult2 \ "commit" \ "sha").as[String]

    waitForPullRequestToHaveSha(testRepo1, prNumber, editSha2, testToken1)

    await(gitHub.getPullRequest(testRepo1, prNumber, testToken1))
  }

  lazy val testInternalPullRequest = Json.obj("pull_request" -> createTestInternalPullRequest())


  lazy val testPullRequests = Map(testExternalPullRequest -> testToken2, testInternalPullRequest -> testToken1)

  lazy val testExternalPullRequestOwnerRepo = (testExternalPullRequest \ "pull_request" \ "base" \ "repo" \ "full_name").as[String]
  lazy val testExternalPullRequestNum = (testExternalPullRequest \ "pull_request" \ "number").as[Int]
  lazy val testExternalPullRequestSha = (testExternalPullRequest \ "pull_request" \ "head" \ "sha").as[String]

  lazy val testInternalPullRequestOwnerRepo = (testInternalPullRequest \ "pull_request" \ "base" \ "repo" \ "full_name").as[String]
  lazy val testInternalPullRequestNum = (testInternalPullRequest \ "pull_request" \ "number").as[Int]
  lazy val testInternalPullRequestSha = (testInternalPullRequest \ "pull_request" \ "head" \ "sha").as[String]

  "we" must {
    "setup stuff" in {
      testRepo1
      testRepo2
      testRepo3
      testFork
      testOrgRepo
      testPullRequests
    }
    "verify the test structure" in {
      withClue(s"$testLogin2 must not be a collaborator on $testRepo1: ") {
        val testRepo1Collaborators = await(gitHub.collaborators(testRepo1, testToken1))
        testRepo1Collaborators must not contain GitHub.GitHubUser(testLogin2)
      }

      withClue(s"$testLogin2 must be a private member of $testOrg: ") {
        val allOrgMembers = await(gitHub.orgMembers(testOrg, testToken1))
        allOrgMembers.value.exists(_.\("login").as[String] == testLogin2) must be (true)
        val publicOrgMembers = await(gitHub.orgMembers(testOrg, testIntegrationToken))
        publicOrgMembers.value.exists(_.\("login").as[String] == testLogin2) must be (false)
      }

      withClue(s"the integration must be installed on $testOrg: ") {
        val integrationInstallations = await(gitHub.integrationInstallations())
        integrationInstallations.value.exists(_.\("account").\("login").as[String] == testOrg) must be (true)
      }

      withClue(s"the integration must be installed on $testLogin1: ") {
        val integrationInstallations = await(gitHub.integrationInstallations())
        integrationInstallations.value.exists(_.\("account").\("login").as[String] == testLogin1) must be (true)
      }
    }
  }

  "GitHub.userRepos" must {
    "fetch all the repos with 10 pages" in {
      (testRepo1, testRepo2, testRepo3) // create 3 repos lazily
      val repos = await(gitHub.userRepos(testLogin1, testToken1, 1))
      repos.value.length must be >= 3
    }
    "fetch all the repos without paging" in {
      val repos = await(gitHub.userRepos(testLogin1, testToken1, Int.MaxValue))
      repos.value.length must be >= 3
    }
    "fetch all the repos with 2 pages" in {
      val repos = await(gitHub.userRepos(testLogin1, testToken1, 2))
      repos.value.length must be >= 3
    }
  }

  "GitHub.userInfo" must {
    "fetch the userInfo" in {
      val userInfo = await(gitHub.userInfo(testToken1))
      (userInfo \ "login").asOpt[String] must contain (testLogin1)
    }
  }

  "GitHub.getPullRequest" must {
    "get a PR" in {
      val pr = await(gitHub.getPullRequest(testExternalPullRequestOwnerRepo, testExternalPullRequestNum, testToken1))
      (pr \ "id").as[Int] must equal ((testExternalPullRequest \ "pull_request" \ "id").as[Int])
    }
  }

  "GitHub.updatePullRequestStatus" must {
    "update a PR" in {
      val pr = await(gitHub.getPullRequest(testExternalPullRequestOwnerRepo, testExternalPullRequestNum, testToken1))
      val mergeState = (pr \ "mergeable_state").as[String]
      val sha = (pr \ "head" \ "sha").as[String]
      val state = if (mergeState == "clean") "pending" else "success"
      val statusCreate = await(gitHub.createStatus(testExternalPullRequestOwnerRepo, sha, state, "https://salesforce.com", "This is only a test", "salesforce-cla:GitHubSpec", testToken1))
      (statusCreate \ "state").as[String] must equal (state)
    }
  }

  "GitHub.pullRequestCommits" must {
    "get the commits on a PR" in {
      val commits = await(gitHub.pullRequestCommits(testExternalPullRequestOwnerRepo, testExternalPullRequestNum, testToken1))
      commits.value.length must be > 0
    }
  }

  "GitHub.collaborators" must {
    "get the collaborators on a repo" in {
      val collaborators = await(gitHub.collaborators(testExternalPullRequestOwnerRepo, testToken1))
      collaborators must contain (GitHub.GitHubUser(testLogin1))
    }
    "work with the Integration" in {
      val collaborators = await(gitHub.collaborators(testRepo1, testIntegrationToken))
      collaborators must contain (GitHub.GitHubUser(testLogin1))
    }
    "see hidden collaborators via the Integration" in {
      val collaborators = await(gitHub.collaborators(testOrgRepo, testIntegrationTokenOrg))
      collaborators must contain (GitHub.GitHubUser(testLogin2))
    }
  }

  "GitHub.commentOnIssue" must {
    "comment on an issue" in {
      val commentCreate = await(gitHub.commentOnIssue(testExternalPullRequestOwnerRepo, testExternalPullRequestNum, "This is only a test.", testToken1))
      (commentCreate \ "id").asOpt[Int] must be ('defined)
    }
  }

  "GitHub.userOrgs" must {
    "include the orgs" in {
      val userOrgs = await(gitHub.userOrgs(testToken1))
      userOrgs.value.map(_.\("login").as[String]) must contain (testOrg)
    }
  }

  "GitHub.allRepos" must {
    "include everything" in {
      val repo = Random.alphanumeric.take(8).mkString
      val ownerRepo = (await(gitHub.createRepo(repo, Some(testOrg))(testToken1)) \ "full_name").as[String]
      waitForRepo(ownerRepo, testToken1)
      val repos = await(gitHub.allRepos(testToken1))
      repos.value.map(_.\("full_name").as[String]) must contain (ownerRepo)
      val deleteResult = await(gitHub.deleteRepo(ownerRepo)(testToken1))
      deleteResult must equal (())
    }
  }

  "GitHub.pullRequests" must {
    "get the pull requests" in {
      val pullRequestsInRepo = await(gitHub.pullRequests(testExternalPullRequestOwnerRepo, testToken1))
      pullRequestsInRepo.value.length must be > 0
    }
    "be able to filter" in {
      val closedPullRequests = await(gitHub.pullRequests(testExternalPullRequestOwnerRepo, testToken1, Some("closed")))
      closedPullRequests.value.length must equal (0)
    }
  }

  "GitHub.pullRequestsToValidate" must {
    lazy val testPullRequest = (testInternalPullRequest \ "pull_request").as[JsObject]
    "work" in {
      val pullRequestsToValidate = await(gitHub.pullRequestsToValidate(testPullRequest, testIntegrationToken))
      pullRequestsToValidate must not be empty
    }
    "not include closed pull requests" in {
      val closedPullRequest = testPullRequest + ("state" -> JsString("closed"))
      val pullRequestsToValidate = await(gitHub.pullRequestsToValidate(closedPullRequest, testIntegrationToken))
      pullRequestsToValidate must be (empty)
    }
  }

  "GitHub.commitStatus" must {
    "get the commit status" in {
      val sha = (testExternalPullRequest \ "pull_request" \ "head" \ "sha").as[String]
      await(gitHub.createStatus(testExternalPullRequestOwnerRepo, sha, "failure", "http://asdf.com", "asdf", "salesforce-cla:GitHubSpec", testToken1))
      val commitStatus = await(gitHub.commitStatus(testExternalPullRequestOwnerRepo, sha, testToken1))
      (commitStatus \ "state").as[String] must equal ("failure")
    }
  }

  "GitHub.issueComments" must {
    "get the issue comments" in {
      await(gitHub.commentOnIssue(testExternalPullRequestOwnerRepo, testExternalPullRequestNum, "This is only a test.", testToken1))
      val issueComments = await(gitHub.issueComments(testExternalPullRequestOwnerRepo, testExternalPullRequestNum, testToken1))
      issueComments.value.length must be > 0
    }
  }

  "GitHub.applyLabel" must {
    "apply a label to issue" in {
      val appliedLabels = await(gitHub.applyLabel(testExternalPullRequestOwnerRepo, "foobar", testExternalPullRequestNum, testToken1))
      appliedLabels.value.map(_.\("name").as[String]) must contain ("foobar")
    }

    "work with non-existent labels" in {
      val appliedLabels = await(gitHub.applyLabel(testExternalPullRequestOwnerRepo, "asdfasdf", testExternalPullRequestNum, testToken1))
      (appliedLabels.head.get \ "name").as[String] must equal ("asdfasdf")
    }

    "have the right color" in {
      val (labelName, labelColor) = gitHub.labels.head
      val appliedLabels = await(gitHub.applyLabel(testExternalPullRequestOwnerRepo, labelName, testExternalPullRequestNum, testToken1))
      val appliedLabel = appliedLabels.value.find(_.\("name").as[String] == labelName).get
      (appliedLabel \ "color").as[String] must equal (labelColor)
    }
  }

  "GitHub.getIssueLabels" must {
    "get labels on an issue" in {
      val issueLabels = await(gitHub.getIssueLabels(testExternalPullRequestOwnerRepo, testExternalPullRequestNum, testToken1))
      issueLabels.value.map(_.\("name").as[String]) must contain ("foobar")
    }
  }

  "GitHub.removeLabel" must {
    "remove a label from issue" in {
      val removedLabel = await(gitHub.removeLabel(testExternalPullRequestOwnerRepo, "foobar", testExternalPullRequestNum, testToken1))
      removedLabel must equal (())
    }
  }

  "GitHub.addOrgWebhook" must {
    "create an org Webhook" in {
      val status = await(gitHub.addOrgWebhook(testOrg, Seq("pull_request"), "http://localhost:9000/foobar", "json", testToken1))
      (status \ "active").as[Boolean] must be (true)
    }
  }

  "GitHub.orgWebhooks" must {
    "get the org webhooks" in {
      val webhooks = await(gitHub.orgWebhooks(testOrg, testToken1))
      webhooks.value.exists(_.\("config").\("url").as[String] == "http://localhost:9000/foobar") must be (true)
    }
  }

  "GitHub.addOrgWebhook" must {
    "delete an org Webhook" in {
      val webhooks = await(gitHub.orgWebhooks(testOrg, testToken1))
      val deletes = webhooks.value.filter(_.\("config").\("url").as[String] == "http://localhost:9000/foobar").map { webhook =>
        val hookId = (webhook \ "id").as[Int]
        await(gitHub.deleteOrgWebhook(testOrg, hookId, testToken1))
      }
      deletes.size must be > 0
    }
  }

  "GitHub.userOrgMembership" must {
    "get the users org membership" in {
      val membership = await(gitHub.userOrgMembership(testOrg, testToken1))
      (membership \ "role").asOpt[String] must be ('defined)
    }
  }

  "GitHub.orgMembers" must {
    "get the org members" in {
      val members = await(gitHub.orgMembers(testOrg, testToken1))
      members.value.length must be > 0
    }
  }

  "GitHub.repoCommit" must {
    "get the repo commit" in {
      val commit = await(gitHub.repoCommit(testExternalPullRequestOwnerRepo, testExternalPullRequestSha, testToken1))
      (commit \ "sha").as[String] must equal (testExternalPullRequestSha)
    }
  }

  "GitHub.repoCommits" must {
    "get the repo commits" in {
      val commits = await(gitHub.repoCommits(testExternalPullRequestOwnerRepo, testToken1))
      commits.value.length must be > 0
    }
  }

  "GitHub.jwtEncode" must {
    "work" in {
      val claim = JwtClaim(subject = Some("test"))
      val encoded = gitHub.jwtEncode(claim)
      val Success(decoded) = JwtJson.decode(encoded, gitHub.integrationKeyPair.getPublic)

      decoded.subject must contain ("test")
    }
  }

  "GitHub.installationAccessTokens" must {
    "work" in {
      val result = await(gitHub.installationAccessTokens(testIntegrationInstallationId))
      (result \ "token").asOpt[String] must be ('defined)
    }
  }

  "GitHub.integrationInstallations" must {
    "work" in {
      val integrations = await(gitHub.integrationInstallations())
      integrations.value.length must be > 0
    }
  }

  "GitHub.installationRepositories" must {
    "work" in {
      val token = (await(gitHub.installationAccessTokens(testIntegrationInstallationId)) \ "token").as[String]
      val repos = await(gitHub.installationRepositories(token))
      repos.value.length must be > 0
    }
  }

  // note that ordering is important here because we validate the same PR multiple times
  "GitHub.pullRequestsToBeValidated" must {
    lazy val pullRequest = createTestInternalPullRequest()
    lazy val sha = (pullRequest \ "head" \ "sha").as[String]
    lazy val number = (pullRequest \ "number").as[Int]

    "work" in {
      val pullRequestsToBeValidated = await(gitHub.pullRequestsToBeValidated(testLogin1))
      pullRequestsToBeValidated must be (empty)
    }
    "include failure state pull requests" in {
      await(gitHub.createStatus(testRepo1, sha, "failure", "http://foo.com", "testing", "salesforce-cla", testToken1))

      waitForCommitState(testRepo1, sha, "failure", testToken1)

      val pullRequestsToBeValidated = await(gitHub.pullRequestsToBeValidated(testLogin1))
      pullRequestsToBeValidated must not be empty
    }
    "not include closed pull requests" in {
      await(gitHub.closePullRequest(testRepo1, number, testToken1))

      val pullRequestsToBeValidatedPostClose = await(gitHub.pullRequestsToBeValidated(testLogin1))
      pullRequestsToBeValidatedPostClose must be (empty)
    }
  }

  "GitHub.pullRequestUserCommitters" must {
    "work" in {
      val pullRequestUserCommitters = await(gitHub.pullRequestUserCommitters(testInternalPullRequestOwnerRepo, testInternalPullRequestNum, testInternalPullRequestSha, testIntegrationToken))
      pullRequestUserCommitters must equal (Set(GitHub.GitHubUser(testLogin1)))
    }
    "fail with non-github user contributors" in {
      // todo: but hard to simulate
      cancel()
    }
  }

  "GitHub.externalContributorsForPullRequest" must {
    "not include repo collaborators" in {
      val externalContributors = await(gitHub.externalContributorsForPullRequest(testInternalPullRequestOwnerRepo, testInternalPullRequestNum, testInternalPullRequestSha, testIntegrationToken))
      externalContributors must be ('empty) // todo: not include the internal user
    }
  }

  // note that ordering is important here because we validate the same PR multiple times
  "GitHub.validatePullRequests" must {
    "work with integrations for pull requests with only internal contributors and/or bots" in {
      val pullRequestsViaIntegration = Map(testInternalPullRequest -> testIntegrationToken)

      val validationResultsFuture = gitHub.validatePullRequests(pullRequestsViaIntegration, "http://asdf.com/", statusUrlF) { _ =>
        Future.successful(Set.empty[ClaSignature])
      }

      val validationResults = await(validationResultsFuture)
      validationResults.size must equal (1)
      (validationResults.head._3 \ "creator" \ "login").as[String].endsWith("[bot]") must be (true)
      (validationResults.head._3 \ "state").as[String] must equal ("success")

      val labels = await(gitHub.getIssueLabels(testInternalPullRequestOwnerRepo, testInternalPullRequestNum, testIntegrationToken))
      labels.value must be ('empty)

      val issueComments = await(gitHub.issueComments(testInternalPullRequestOwnerRepo, testInternalPullRequestNum, testIntegrationToken))
      issueComments.value must be ('empty)
    }
    "not comment on a pull request when the external contributors have signed the CLA" in {
      val pullRequestsViaIntegration = Map(testExternalPullRequest -> testIntegrationToken)

      val validationResultsFuture = gitHub.validatePullRequests(pullRequestsViaIntegration, "http://asdf.com/", statusUrlF) { _ =>
        Future.successful(Set(ClaSignature(1, testLogin2, LocalDateTime.now(), "1.0")))
      }

      val validationResults = await(validationResultsFuture)
      validationResults.size must equal (1)
      (validationResults.head._3 \ "state").as[String] must equal ("success")

      val labels = await(gitHub.getIssueLabels(testExternalPullRequestOwnerRepo, testExternalPullRequestNum, testIntegrationToken))
      labels.value.exists(_.\("name").as[String] == "cla:signed") must be (true)

      val issueComments = await(gitHub.issueComments(testExternalPullRequestOwnerRepo, testExternalPullRequestNum, testIntegrationToken))
      issueComments.value.count(_.\("user").\("login").as[String].endsWith("[bot]")) must equal (0)
    }
    "work with integrations for pull requests with external contributors" in {
      val pullRequestsViaIntegration = Map(testExternalPullRequest -> testIntegrationToken)

      val validationResultsFuture = gitHub.validatePullRequests(pullRequestsViaIntegration, "http://asdf.com/", statusUrlF) { _ =>
        Future.successful(Set.empty[ClaSignature])
      }

      val validationResults = await(validationResultsFuture)
      validationResults.size must equal (1)
      (validationResults.head._3 \ "creator" \ "login").as[String].endsWith("[bot]") must be (true)
      (validationResults.head._3 \ "state").as[String] must equal ("failure")

      val labels = await(gitHub.getIssueLabels(testExternalPullRequestOwnerRepo, testExternalPullRequestNum, testIntegrationToken))
      labels.value.exists(_.\("name").as[String] == "cla:missing") must be (true)

      val issueComments = await(gitHub.issueComments(testExternalPullRequestOwnerRepo, testExternalPullRequestNum, testIntegrationToken))
      issueComments.value.count(_.\("user").\("login").as[String].endsWith("[bot]")) must equal (1)
    }
    "not comment twice on the same pull request" in {
      val pullRequestsViaIntegration = Map(testExternalPullRequest -> testIntegrationToken)

      await(gitHub.validatePullRequests(pullRequestsViaIntegration, "http://asdf.com/", statusUrlF)(_ => Future.successful(Set.empty[ClaSignature])))
      await(gitHub.validatePullRequests(pullRequestsViaIntegration, "http://asdf.com/", statusUrlF)(_ => Future.successful(Set.empty[ClaSignature])))

      val issueComments = await(gitHub.issueComments(testExternalPullRequestOwnerRepo, testExternalPullRequestNum, testToken1))
      issueComments.value.count(_.\("user").\("login").as[String].endsWith("[bot]")) must equal (1)
    }
  }

  "integrationAndUserOrgs" should {
    "work" in {
      val integrationAndUserOrgs = await(gitHub.integrationAndUserOrgs(testToken1))
      integrationAndUserOrgs.get(testOrg) must be ('defined)
    }
  }

  "repoContributors" should {
    "work" in {
      val repoContributors = await(gitHub.repoContributors(testOrgRepo, testIntegrationToken)).map(_.contributor)
      val repoName = testOrgRepo.split("/").last
      repoContributors contains GitHubUser(testLogin1)
      repoContributors contains UnknownCommitter(Some(repoName), Some(s"$repoName@$repoName.com"))
    }
  }

  "UnknownCommitter.toStringOpt" should {
    "obfuscate emails" in {
      UnknownCommitter(None, Some("asdf@foobar.com")).toStringOpt() must contain ("a***@f***.com")
      UnknownCommitter(None, Some("asdf@foo.bar.com")).toStringOpt() must contain ("a***@f***.b***.com")
    }
  }

  "we" must {
    "cleanup" in {
      if (sys.env.get("DO_NOT_CLEANUP").isEmpty) {
        await(gitHub.deleteRepo(testFork)(testToken2))
        await(gitHub.deleteRepo(testRepo1)(testToken1))
        await(gitHub.deleteRepo(testRepo2)(testToken1))
        await(gitHub.deleteRepo(testRepo3)(testToken1))
        await(gitHub.deleteRepo(testOrgRepo)(testToken1))
      }
    }
  }

}

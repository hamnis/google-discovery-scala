package org.typelevel.sbt

import com.github.sbt.git.GitPlugin
import com.github.sbt.git.SbtGit.git
import org.typelevel.sbt.TypelevelKernelPlugin._
import org.typelevel.sbt.kernel.GitHelper
import org.typelevel.sbt.kernel.V
import sbt._

import scala.util.Try

import Keys._

//make sbt-typelevel versioning suck less
object MyVersioningPlugin extends AutoPlugin {
  override def requires = GitPlugin
  override def trigger = allRequirements

  object autoImport {
    lazy val myBaseVersion =
      settingKey[String]("The base version for the series your project is in. e.g., 0.2, 3.5")
    lazy val myUntaggedAreSnapshots =
      settingKey[Boolean](
        "If true, an untagged commit is given a snapshot version, e.g. 0.4-00218f9-SNAPSHOT. If false, it is given a release version, e.g. 0.4-00218f9. (default: true)")

    lazy val myLatestVersion = settingKey[Option[String]](
      "The latest tagged version on this branch. Priority is given to the latest stable version, but if you have tagged a binary-breaking prelease version (such as a milestone or release candidate), that will be selected instead. If applicable, this will be the current tagged version.")

    lazy val myLatestPreReleaseVersion = settingKey[Option[String]](
      "The latest tagged version on this branch, including milestones and release candidates. If applicable, this will be the current tagged version.")
  }

  import autoImport._

  override def buildSettings: Seq[Setting[_]] = Seq(
    versionScheme := Some("early-semver"),
    myUntaggedAreSnapshots := true,
    isSnapshot := {
      val isUntagged = taggedVersion.value.isEmpty
      val dirty = git.gitUncommittedChanges.value
      dirty || (isUntagged && myUntaggedAreSnapshots.value)
    },
    git.gitCurrentTags := {
      // https://docs.github.com/en/actions/learn-github-actions/environment-variables
      // GITHUB_REF_TYPE is either `branch` or `tag`
      if (sys.env.get("GITHUB_REF_TYPE").contains("branch"))
        // we are running in a workflow job that was *not* triggered by a tag
        // so, we discard tags that would affect our versioning
        git.gitCurrentTags.value.flatMap {
          case V.Tag(_) => None
          case other => Some(other)
        }
      else
        git.gitCurrentTags.value
    },
    version := {
      import scala.sys.process._

      val baseV = V(myBaseVersion.value)
        .filter(v => v.patch.isEmpty && v.prerelease.isEmpty)
        .getOrElse(sys.error(s"tlBaseVersion must be of form x.y: ${myBaseVersion.value}"))

      val taggedV =
        if (git.gitUncommittedChanges.value)
          None // tree is dirty, so ignore the tags
        else taggedVersion.value.map(_.toString)

      var version = taggedV.getOrElse {
        // No tag, so we build our version based on this commit

        val latestInSeries = GitHelper
          .previousReleases(true)
          .filterNot(_.isPrerelease) // TODO Ordering of pre-releases is arbitrary
          .headOption
          .flatMap {
            case previous if previous > baseV =>
              sys.error(s"Your tlBaseVersion $baseV is behind the latest tag $previous")
            case previous if baseV.isSameSeries(previous) =>
              Some(previous)
            case _ => None
          }

        // version here is the prefix used further to build a final version number
        var version = latestInSeries.fold(myBaseVersion.value)(_.toString)

        // Looks for the distance to latest release in this series
        latestInSeries.foreach { latestInSeries =>
          Try(s"git describe --tags --match v$latestInSeries".!!.trim)
            .collect { case Description(distance) => distance }
            .foreach(distance => version += s"-$distance")
        }

        version
      }

      V(version) match {
        case None =>
          sys.error(s"version must be semver format: $version")
        case Some(v) if !(v.isSameSeries(baseV) || v >= baseV) =>
          sys.error(s"Your current version $version cannot be less than tlBaseVersion $baseV")
        case _ => // do nothing
      }

      if (isSnapshot.value) version += "-SNAPSHOT"

      version
    },
    myLatestVersion := currentRelease.value,
    myLatestPreReleaseVersion := currentPreRelease.value
  )

  private val Description = """^.*-(\d+)-[a-zA-Z0-9]+$""".r

  private def taggedVersion = Def.setting {
    git.gitCurrentTags.value.collect { case V.Tag(v) => v }.sorted.lastOption
  }
}

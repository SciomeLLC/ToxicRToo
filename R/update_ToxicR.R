#' Update the package 'ToxicRToo'
#'
#' @title update_ToxicRToo - downloads and tries to install the latest
#' version of the package from github
#' Updates the current package 'ToxicRToo' by installing the
#' most recent version of the package from GitHub
#' This function requires installing Package 'remotes' v2.4.2
#' (or possibly a higher version) by Csardi et al. (2021),
#' <https://cran.r-project.org/package=remotes>
#'
#' @param force logical. If \code{force = TRUE}, force installing the
#' update. If \code{force = FALSE}, do not force installing the update.
#' By default, \code{force = TRUE}.
#' @param upgrade_other_pkg input for the \code{upgrade} argument to
#' be passed on to \code{remotes::install_github}.
#' One of "default", "ask", "always", "never", TRUE, or FALSE.
#' "default" respects the value of the R_REMOTES_UPGRADE environment
#' variable if set, and falls back to "ask" if unset.
#' "ask" prompts the user for which out of date packages to upgrade.
#' For non-interactive sessions "ask" is equivalent to "always".
#' TRUE and FALSE correspond to "always" and "never" respectively.
#' By default, \code{upgrade_other_pkg = FALSE}.
#' @param confirm logical. If \code{confirm = TRUE}, the user will
#' need to confirm the update. If \code{confirm = FALSE}, the confirmation
#' step will be skipped. By default, \code{confirm = TRUE}.
#' @return there will be no output from this function. Rather, executing
#' this function will update the current 'ToxicRToo' package by installing
#' the most recent version of the package from GitHub.
#' @examples
#' \dontrun{
#' update_ToxicRToo()
#' }
#' @export
update_ToxicRToo <- function(
    force = TRUE,
    upgrade_other_pkg = FALSE,
    confirm = TRUE) {
    # 6 possible cases
    # 1. error in getting the current package version -> yes
    # 2. error in getting the github package version -> yes
    # 3. current package version < github package version -> yes
    # 4. current package version > github package version -> no
    # 5. current package version == github package version -> no
    # 6. the user forces the update -> yes
    # in short, notify the option to update unless the version numbers match
    if (force == TRUE) {
        # unload the package ToxicRToo
        while ("package:ToxicRToo" %in% search()) {
            unloadNamespace("ToxicRToo")
        }
        message(
            "Please run the following: \n",
            "remotes::install_github(\"SciomeLLC/ToxicRToo\",
            force = force, upgrade = upgrade_other_pkg)"
        )
    } else {
        # get version of the currently installed package
        current_pkg_version <- tryCatch(
            as.character(utils::packageVersion("kim")),
            error = function(e) "unknown"
        )
        # github url
        github_url <- message(
            "https://raw.githubusercontent.com/SciomeLLC/",
            "ToxicRToo/github-actions-build/DESCRIPTION"
        )
        # get github description or handle errors
        github_pkg_desc <- tryCatch(
            readLines(github_url),
            warning = function(w) {
                "github_desc_read_fail"
            },
            error = function(e) {
                "github_desc_read_fail"
            }
        )
        # get the version number of github version
        if (identical(github_pkg_desc, "github_desc_read_fail")) {
            github_pkg_version <- "unknown"
        } else {
            github_pkg_version <- gsub(
                ".*ersion: ", "", github_pkg_desc[
                    grep("ersion", github_pkg_desc)
                ]
            )
        }
        # compare versions
        compare_version_result <- tryCatch(
            utils::compareVersion(
                current_pkg_version, github_pkg_version
            ),
            warning = function(w) {
                999
            }, # 999 indicates no need for update
            error = function(e) {
                999
            }
        )
        # skip update for case 5
        if (
            current_pkg_version != "unknown" &&
                github_pkg_version != "unknown" &&
                compare_version_result == 0
        ) {
            message(
                "Current version of 'ToxicRToo': v", current_pkg_version,
                " (same as the most recent version available through GitHub)."
            )
        } else if (
            # skip update for case 4
            current_pkg_version != "unknown" &&
                github_pkg_version != "unknown" &&
                compare_version_result > 0) {
            message(
                "Current version of 'ToxicRToo': v", current_pkg_version,
                " (probably the most recent version available through GitHub)."
            )
        } else {
            # confirm update
            if (confirm == TRUE) {
                # ask the user to confirm
                user_reply <- utils::menu(
                    c("Yes.", "No."),
                    title = "\nDo you want to try to update the package
                    'ToxicRToo'?"
                )
            } else {
                # if not asked, assume the user wants to update
                user_reply <- 1
            }
            # update if user wants the update
            if (user_reply == 1) {
                # unload the package kim
                while ("package:ToxicRToo" %in% search()) {
                    unloadNamespace("ToxicRToo")
                }
                message(
                    "Please run the following: \n",
                    "remotes::install_github(\"SciomeLLC/ToxicRToo\",
            force = force, upgrade = upgrade_other_pkg)"
                )
            }
        }
    }
}

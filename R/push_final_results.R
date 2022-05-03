#' push_final_results
#'
#' This function takes a source repository, a destination repository, and a list of files to be copied
#' It commits and pushes any changes to the source repository, and then copies the files from the
#' source repository to the destination repository. After files are copied, they are staged, committed and pushed to the
#' destination repository.
#'
#' @param source_repo The file path for the git repository that is the source of the files
#' @param source_subdir The name of the subdirectory in which the source files are located. Default is NULL
#' @param dest_repo The file path for the destination git repository
#' @param dest_subdir The name of the subdirectory into which the source files should be moved. Default is NULL
#' @param source_files A vector of files to be moved from the source repository to the destination repository
#' @param commit_message Message to attach to the commits for both source and destination repository (optional)
#'
#' @return
#' @export
#'
#' @examples
#'
push_final_results <-
  function(
    source_repo,
    source_subdir = NULL,
    dest_repo,
    dest_subdir = NULL,
    source_files,
    commit_message = paste("source repo to destination repo", Sys.Date())
  ) {
    # Convert file paths

    # Convert strings to file path
    source_repo <- file.path(source_repo)
    dest_repo <- file.path(dest_repo)

    # If files are located in a sub-directory, create file path here
    if (!is.null(source_subdir)) {
      source_fullpath <- file.path(source_repo, source_subdir)
    } else if (is.null(source_subdir)) {
      source_fullpath <- file.path(source_repo)
    }

    # If files are to be copied into a sub-directory, create file path here
    if (!is.null(dest_subdir)) {
      dest_fullpath <- file.path(dest_repo, dest_subdir)
    } else if (is.null(dest_subdir)) {
      dest_fullpath <- file.path(dest_repo)
    }

    # Confirm all file paths exist
    if (file.exists(source_repo) == FALSE) {

      stop(glue('The file path for the source repository "{source_repo}" does not exist.'),
           call. = FALSE)

    } else if (file.exists(dest_repo) == FALSE) {

      stop(glue('The file path for the destination repository "{dest_repo}" does not exist.'),
           call. = FALSE)

    } else if (file.exists(source_fullpath) == FALSE) {

      stop(glue('The source repository sub-directory "{source_fullpath} does not exist.'),
           call. = FALSE)

    } else if (file.exists(dest_fullpath) == FALSE) {

      stop(glue('The destination repository sub-directory "{dest_fullpath}" does not exist.'),
           call. = FALSE)
    }

    # Save out list of files with full file path
    source_files_fullpath <-
      purrr::map(source_files, ~ file.path(source_fullpath, .x))

    # Confirm all files exist
    for (i in source_files_fullpath) {

      if (file.exists(i) == FALSE) {

        stop(glue('A file in the source repository ("{i}") does not exist.'),
             call. = FALSE)

      }

    }

    # Update source repository

    # Pull results from source repository to avoid merge conflicts
    gert::git_pull(repo = source_repo)

    # Stage and commit any changed or new files in source repo
    gert::git_add(files = "*", repo = source_repo)
    gert::git_commit(message = commit_message, repo = source_repo)

    # Push results to source repository first to ensure consistency
    gert::git_push(repo = source_repo)

    # Update destination repository

    # Pull results to avoid merge conflicts
    gert::git_pull(repo = dest_repo)

    # Copy results files from source repo folder to destination repo folder/subfolder
    file.copy(
      from = source_files_fullpath,
      to = dest_fullpath
    )

    # Stage and commit all new files
    gert::git_add(files = "*", repo = dest_repo)
    gert::git_commit(message = commit_message, repo = dest_repo)

    # Push files to results repo
    gert::git_push(repo = dest_repo)

  }

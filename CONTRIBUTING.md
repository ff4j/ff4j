# Contributing

When contributing to this repository, please first discuss the change you wish to make via issue,
email, or any other method with the owners of this repository before making a change. 

Please note we have a [Code of Conduct](CODE_OF_CONDUCT.md), please follow it in all your interactions with the project.

## <a name="issue"></a> Found an Issue?
If you find a bug in the source code or a mistake in the documentation, you can help us by
[submitting an issue](#submit-issue) to the GitHub Repository. Even better, you can
[submit a Pull Request](#submit-pr) with a fix.

## <a name="feature"></a> Want a Feature?
You can *request* a new feature by [submitting an issue](#submit-issue) to the GitHub
Repository. If you would like to *implement* a new feature, please submit an issue with
a proposal for your work first, to be sure that we can use it.

* **Small Features** can be crafted and directly [submitted as a Pull Request](#submit-pr).

## <a name="submit"></a> Contribution Guidelines

### <a name="submit-issue"></a> Submitting an Issue
Before you submit an issue, search the archive, maybe your question was already answered.

If your issue appears to be a bug, and hasn't been reported, open a new issue.
Help us to maximize the effort we can spend fixing issues and adding new
features, by not reporting duplicate issues.  Providing the following information will increase the
chances of your issue being dealt with quickly:

* **Overview of the Issue** - if an error is being thrown a non-minified stack trace helps
* **Motivation for or Use Case** - explain what are you trying to do and why the current behavior is a bug for you
* **Reproduce the Error** - provide a live example or a unambiguous set of steps
* **Suggest a Fix** - if you can't fix the bug yourself, perhaps you can point to what might be
  causing the problem (line of code or commit)

### <a name="submit-pr"></a> Submitting a Pull Request (PR)
Before you submit your Pull Request (PR) consider the following guidelines:

* Search the repository (https://github.com/bechbd/[repository-name]/pulls) for an open or closed PR that relates to your submission. You don't want to duplicate effort.

* Create a fork of the repo
	* Navigate to the repo you want to fork
	* In the top right corner of the page click **Fork**:
	![](https://help.github.com/assets/images/help/repository/fork_button.jpg)

* Make your changes in the forked repo
* Commit your changes using a descriptive commit message
* In GitHub, create a pull request: https://help.github.com/en/articles/creating-a-pull-request-from-a-fork
* If we suggest changes then:
  * Make the required updates.
  * Rebase your fork and force push to your GitHub repository (this will update your Pull Request):

    ```shell
    git rebase master -i
    git push -f
    ```

That's it! Thank you for your contribution!
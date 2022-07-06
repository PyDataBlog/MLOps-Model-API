##1.5 Tracking Changes##

**How does tracking and adding changes make developers' lives easier?**

>Tracking and adding changes allow developers to break the giant work of projects into smaller chunks which can be updated and modified discretely from the entire project. The tracked changes make it easy to see who's made what changes and when, as well as making it efficient to walk changes back to an earlier state in case of a mistake.

**What is a commit?**

>A commit is frozen version of a repository that can be returned to, modified, and referenced at any point in the future. It's like a beefed-up version of saving a file that includes all files and doesn't overwrite previous saves. 

**What are the best practices for commit messages?**

>Good commit messages should start with a short 50 character or less message written in the imperative present tense. This is a little confusing, but it helps to think of this short preface as describing what applying the commit to the master branch *will* do rather than the work that you *did*. After this short descriptor, leave a blank line before writing out a more detailed description of the commit message if desired.

**What does the HEAD^ argument mean?**

>This argument can be used to refer to other commits. HEAD is the current commit and HEAD^ is the previous commit.

**What are the 3 stages of a git change and how do you move a file from one stage to the other?**

>The first stage is modifying the file. The second stage is staging the changed file to go into the next commit. To do this you do `git add THE_FILENAME_TO_ADD`(or `git add .` to add all the files in the directory. The third stage is committing the staged changes. To do this you do `git commit -m "Leaves a good commit message."`

**Write a handy cheatsheet of the commands you need to commit your changes?**

###Commit Cheat Sheet

```
git add THE_FILENAME_TO_ADD
git commit -m "Leaves a good commit message."
```

###Pull Request Cheat Sheet

```
git checkout master
git pull origin master
git checkout feature-branch
git merge master
git push origin feature-branch
```
Make a Pull Request

Merge Changes

Delete Branch
```
git checkout master
git pull origin master
git branch -d feature-branch
```

**What is a pull request and how do you create and merge one?**

>A pull request is a notification that you've made changes that are ready to be merged into the master branch from the feature branch you've been working on. You create one by pushing your feature-branch to the origin, then navigating to that branch in GitHub and clicking on Pull Requests. Ideally, the branch can merge without any conflicts and you'll be then given the option to delete the feature branch you've been working on (as the code is now available in master).

**Why are pull requests preferred when working with teams?**

>Pull requests make it easier for teams to work together as they allow many different feature branches for different team members to exist and be modified at once. Then team members can look over pull requests and merge them one at a time into the master branch in order to prevent (or at least minimize) conflicting code versions. 

**Add your reflection:**

>I learned a lot in this exercise. For example: I had no idea there were best practices for commit messages, and now I know I was doing them completely backward by writing them in the past tense. I also struggle with remembering all the steps of a pull request, so I made a cheat sheet of that for myself in addition to the commit cheat sheet. My previous git usage was all just projects for myself, so I was always writing code in the master branch and just committing those changes without ever branching. I had some inkling I wasn't doing it right, but now I've at least done it the "right" way a couple of times over the course of this exercise to start building up the muscle memory for it.

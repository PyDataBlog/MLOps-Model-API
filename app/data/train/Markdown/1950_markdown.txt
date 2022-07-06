+ *Paste a link to your [USERNAME].github.io repository.*
http://ello21.github.io/

+ *Explain how to create a repository on GitHub and clone the repository to your local computer to a non-technical person.*
  - Click on the ``+`` sybmol at the top right, next to your profile pic. Select ``New Repository``
  - Fill in the repository name and options (description, public vs. private, read me, license type).
  - Click ``Create Repository"
  - Now that your repository exists online, we need to get a copy onto your computer. Look for a link on your repository's page that says ``HTTPS`` (top center, or right side). Copy this link.
  - We're going to copy this link into a folder on your computer. Open your computer's terminal or command line and type ``cd`` followed by a space followed by the full path to the folder where you want your repository to be located.
  - Now type the command ``git clone`` followed by a space followed by the ``HTTPS`` link you copied earlier. Hit enter, and your repo is cloned!

+ *Describe what open source means.*
Open source is software which has its source code made available to the user. The user can view and customize the source code freely.

+ *What do you think about Open Source? Does it make you nervous or protective? Does it feel like utopia?*
Open source is valuable and necessary in the development community. Making tools and entire codebases available for other developers to build on allows for more frequent and increasingly complex software to be created. It takes away the need to keep "reinventing the wheel". I would feel protective if I was creating what I believed to be my life's great work, but I understand that if I'm building on others work then I can't reap the benefits alone. If I was embarkingo on a private enterprise then I would accept that all the code would need to be written from scratch. I wouldn't call Open Source "utopia", just quid pro quo.

+ *Assess the importance of using licenses.*
An example of licensing importance: You're a digital artist who publishes free wall papers for people to download off your website. Your intention is to share your work, but then someone starts taking your images, modifying them, putting their own logo on them, and reselling them. A license that specifices "no modifications or commercial use" would have given you legal protection in this case.

+ *What concepts were solidified in the challenge? Did you have any "aha" moments? What did you struggle with?*
This challenge reinforced the need to update the master branch on my computer before attempting to delete the feature branch after a merge is completed. Previously, I received an error that the new branch was not fully merged. This was because the master branch on my computer had not been updated, and was "out of sync" with the feature branch. Now it will become a habit to do ``git pull origin master`` before attempting to delete the feature branch.

+ *Did you find any resources on your own that helped you better understand a topic? If so, please list it.*
This challenge was pretty straightforward. I didn't need any outside resources.
# Example ServerSpec to InSpec Migration
[![CircleCI](https://circleci.com/gh/bonusbits/example_serverspec_to_inspec.svg?style=shield)](https://circleci.com/gh/bonusbits/example_serverspec_to_inspec)
[![Join the chat at https://gitter.im/bonusbits/example_serverspec_to_inspec](https://badges.gitter.im/bonusbits/example_serverspec_to_inspec.svg)](https://gitter.im/bonusbits/example_serverspec_to_inspec?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

## Purpose
The purpose of this project is to demonstrate migrating ServerSpec to InSpec automated integration tests in a Chef Cookbook. The process is split into three different *[HowTo](https://www.youtube.com/playlist?list=PLy2eDDzDOIEpf6obkRNB_Eikx32b68f8I)* videos I've created and linked below. A git branch for each corresponding training video has been created for easy reference.

## Environment
| Software                  | Version        |
| :--------------------     | :------------- |
| macOS                     | 10.12.4        |
| Docker                    | 17.05.0-ce-rc1 |
| Chef Development Kit      | 1.3.40         |
| kitchen-docker            | 2.6.0          |
| CircleCi                  | 2.0            |
| Amazon Linux Docker Image | 2017.03        |

# Part 1 - Create a ServerSpec Tested Cookbook
The first step is to create and example Chef cookbook that uses ServerSpec integration testing. We'll be setting it up with kitchen-docker Test Kitchen driver.

#### Links
* [Reference Git Branch](https://github.com/bonusbits/example_serverspec_to_inspec/tree/01_serverspec)
* [Written Instructions](https://www.bonusbits.com/wiki/HowTo:Migrate_ServerSpec_to_InSpec_-_Part_1)
* [Walkthrough Video](https://youtu.be/fn_GV9Ejnqc)<br>
[![Part 1](images/part_1_vid_still.png)](https://youtu.be/fn_GV9Ejnqc)

# Part 2 - Convert ServerSpec to InSpec Local Tests
Next, we take the previously created Chef Cookbook from [Part 1 Git Branch](https://github.com/bonusbits/example_serverspec_to_inspec/tree/01_serverspec) in our new 02_inspec_local branch and convert the ServerSpec tests to InSpec local tests.

#### Links
* [Reference Git Branch](https://github.com/bonusbits/example_serverspec_to_inspec/tree/02_inspec_local)
* [Written Instructions](https://www.bonusbits.com/wiki/HowTo:Migrate_ServerSpec_to_InSpec_-_Part_2)
* [Walkthrough Video](https://youtu.be/jLJu2fi2z4g)<br>
[![Part 2](images/part_2_vid_still.png)](https://youtu.be/jLJu2fi2z4g)

# Part 3 - Convert Local InSpec Tests to Shared InSpec Profile
Next, we create a new git repository for our new shared InSpec profile and add the local InSpec tests to it from [Part 2 Git Branch](https://github.com/bonusbits/example_serverspec_to_inspec/tree/02_inspec_local). Finally, we demonstrate calling our remote InSpec Profile suite of tests from out Chef Cookbook Test Kitchen configuration.

#### Links
* [Cookbook Reference Git Branch](https://github.com/bonusbits/example_serverspec_to_inspec/tree/03_inspec_remote)
* [Shared InSpec Profile Git Repo](https://github.com/bonusbits/example_shared_inspec)
* [Written Instructions](https://www.bonusbits.com/wiki/HowTo:Migrate_ServerSpec_to_InSpec_-_Part_3)
* [Walkthrough Video](https://youtu.be/S0RvMnQpjXg)<br>
[![Part 3](images/part_3_vid_still.png)](https://youtu.be/S0RvMnQpjXg)

## Other Links
* [YouTube Playlist](https://www.youtube.com/playlist?list=PLy2eDDzDOIEpf6obkRNB_Eikx32b68f8I)
* [Written Instructions Root Article](https://www.bonusbits.com/wiki/HowTo:Migrate_ServerSpec_Integration_Tests_to_InSpec_for_Chef_Cookbook)
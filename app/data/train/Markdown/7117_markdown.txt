# Bower Template
A Visual Studio template for bower development. Including NPM / Gulp / Bower. 

*Latest Version Download: [Bower v0.2.0](https://github.com/Rendxx/BowerTemplate/releases/tag/0.2.0 "Download")*
  
## Install
1. Download the template zip file

2. Move the zip file to Visual Studio custom template directory.  
    Path: *\\My Documents\\Visual Studio Version\\Templates\\ProjectTemplates\\*  
    More information: [How to: Locate and Organize Project and Item Templates][Templates Directory] 

3. Open Visual Studio.  
    Click **File** > **New** > **Project** > **Installed** > **JavaScript**  
    Select **Bower**

4. Wait for Visual Studio to setup NPM Bower and Gulp.  
   If Visual Studio does not automatically setup them, follow step 5 and 6 to do it manually.   
   Otherwise, you are all set. 

5. Open and save **package.json** to install npm packages  
    Open and save **bower.json** to install bower packages  
    Open and save **gulpfile.json** to setup gulp task

6. Run gulp task ```_bowerCopy``` to copy bower packages  
    Run gulp task ```js``` to process JavaScript files  
    Run gulp task ```less``` to process Less files  
    Run gulp task ```_watch``` if you want to setup automatic processing on editing JavaScript/Less files  

7. Open **/test/sample.html** in browser to see the default page.

![preview](https://raw.githubusercontent.com/Rendxx/BowerTemplate/master/screenshot-success.png)  
> Installation success if you see this screen.

## Structure
- **src/**   
  Directory of source files.  
  **src/js/**   
  Directory of **JavaScript** source files.  
  **src/less/**   
  Directory of **LESS** source files.  

- **js/**   
  Directory of bundled and minimized **JavaScript** files.  
  **css/**   
  Directory of **Css** files, bundled and created from LESS files.   
  **lib/**   
  Directory of 3rd-party files, copied from Bower directory.  

- **test/**  
  Directory of test files.  
  You can add your own test files here.  
  **test/sample.html**  
  **test/sample.js**  
  **test/sample.less**  
  Default test files.

- **bundle.json**  
  Use to set bundle / minimization / processing of Bower libraries, JavaScript files and LESS files.  
  **bower.json**  
  Bower file  
  **package.json**  
  NPM file  
  **gulpfile.js**  
  Gulp file  

## Gulp Task
Gulp is used to process JavaScript and LESS files.  
[More information about Gulp][Gulp]

- **js**  
  Process and combine JavaScript files listed in *bundle.json*, minimized files will be created as well.  
  All processed files will be put in *wwwroot\\js\\*

- **less**  
  Process and combine Less files listed in *bundle.json*  
  All processed files will be put in *wwwroot\\less\\*

- **\_watch**  
  Setup automatic processing on editing JavaScript/Less files.  
  This task will run automatically when this project opens after gulp set.

- **\_bowerCopy**  
  Copy selected files from Bower packages. It only need to be ran once after installing Bower packages.  
  This task will run automatically when this project opens after gulp set.

- **\_bowerClear**  
  Clear copied bower libraries. Called by task **\_bowerCopy**.

## Usage
1. **Changing JavaScript/LESS Files**  
   Add / Delete / Edit JavaScript and LESS source files as you wish.

2. **Add Bundle**  
   Edit **bundle.json** to make Gulp task process your source files.

3. **Add Link in HTML File**  
   Don't forget add a link to JavaScript / CSS files in HTML file.


## License 
Copyright &copy; 2015, Rendxx. (MIT License)  
See [LICENSE][] for more info.

[Templates Directory]: https://msdn.microsoft.com/en-us/library/y3kkate1.aspx "How to: Locate and Organize Project and Item Templates"
[Gulp]: https://github.com/gulpjs/gulp/blob/master/docs/getting-started.md "Gulp Doc"
[LICENSE]: https://github.com/Rendxx/JavaScriptHtmlTemplate/blob/master/LICENSE
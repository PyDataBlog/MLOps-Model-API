
'use strict'

let
ugly = require('gulp-uglify')
,gulp = require('gulp')
,watch = require('gulp-watch')
,plumber = require('gulp-plumber')
,newer = require('gulp-newer')
,stylus = require('gulp-stylus')
,jade = require('gulp-jade')
,concat = require('gulp-concat')
,rename = require('gulp-rename')
,runSequence = require('run-sequence')
,_ = require('lodash')
,path = require('path')
,fs = require('fs')
,spawn = require('cross-spawn')

let
cssFolder = __dirname + '/public/css'
,jsFolder = __dirname + '/public/js'
,views = __dirname + '/views'
,stylusOptions = {
	compress: true
}
,uglyOptions = {

}

gulp.task('stylus', function() {

	gulp.src(cssFolder + '/*.styl')
		/*
		.pipe(newer({
			dest: cssFolder
			,map: function(path) {
				return path.replace(/\.styl$/, '.css')
			}
		}))
		*/
		.pipe(plumber())
		.pipe(stylus(stylusOptions))
		.pipe(gulp.dest(cssFolder))

})


gulp.task('ugly', function() {

	gulp.src(jsFolder + '/*.js')
		.pipe(newer({
			dest: jsFolder
			,map: function(path) {
				return path.replace(/\.dev.js$/, '.min.js')
			}
		}))
		.pipe(plumber())
		.pipe(rename(function (path) {
			path.basename = path.basename.replace('.dev', '.min')
		}))
		.pipe(gulp.dest(jsFolder))
		.pipe(ugly(uglyOptions))
		.pipe(gulp.dest(jsFolder))

})

let config = require('./config')
let pack = require('./package.json')
let packj = require('./node_modules/jade-press/package.json')
config.version = pack.version
config.siteDesc = packj.description

gulp.task('jade', function() {

	gulp.src(views + '/*.jade')
		.pipe(plumber())
		.pipe(jade({
			locals: config
		}))
		.pipe(gulp.dest(__dirname))

})

 
gulp.task('server', function (cb) {
	var runner = spawn(
		'node'
		,['server']
		,{
			stdio: 'inherit'
		}
	)

	runner.on('exit', function (code) {
		process.exit(code)
	})

	runner.on('error', function (err) {
		cb(err)
	})
})

gulp.task('watch',  function () {

	runSequence('server')
	watch([cssFolder + '/*.styl', cssFolder + '/parts/*.styl'], function() {
		runSequence('stylus')
	})

	watch(jsFolder, function() {
		runSequence('ugly')
	})

	watch([
			views + '/*.jade'
			,views + '/parts/*.jade'
		], function() {
			runSequence('jade')
		}
	)

})




gulp.task('default', ['watch'])
gulp.task('dist', function() {
	runSequence('stylus', 'ugly', 'jade')
})
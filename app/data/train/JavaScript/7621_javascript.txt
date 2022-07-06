'use strict';

var mongoose = require('mongoose'),
	async = require('async');

var DeleteChildrenPlugin = function(schema, options) {
	schema.pre('remove', function(done) {

		var parentField = options.parentField;
		var childModel = options.childModel;

		if (!parentField) {
			var err = new Error('parentField not defined');
			return done(err);
		}

		if (!childModel) {
			var err = new Error('childModel not defined');
			return done(err);
		}
		// must delete all campuses 

		console.log('model::', parentField, '>', childModel, '::pre::remove::enter');
		var Model = mongoose.model(childModel);
		var conditions = {};
		conditions[parentField] = this._id;
		Model.find(conditions).exec(function(err, results) {
			console.log('model::', parentField, '>', childModel, '::pre::remove::find::enter');
			if (err) {
				return done(err);
			}

			async.each(results, function(campus, deleteNextModel) {
				console.log('model::', parentField, '>', childModel, '::pre::remove::find::each::enter');
				campus.remove(deleteNextModel);
			}, done);

		});

	});
};


var ParentAttachPlugin = function(schema, options) {

	schema.pre('save', function(doneAttaching) {
		var doc = this;
		var parentField = options.parentField;
		var parentModel = options.parentModel;
		var childCollection = options.childCollection;

		if (!doc[parentField]) return doneAttaching();

		var Parent = mongoose.model(parentModel);
		var push = {};
		push[childCollection] = doc._id;
		Parent.findByIdAndUpdate(doc[parentField], {
			$push: push
		}).exec(function(err) {
			
			if (err) {
				console.log('plugin::ParentAttachPlugin::schema::pre::save::findByIdAndUpdate::err', err);
				return doneAttaching(err);
			}

			console.log('plugin::ParentAttachPlugin::schema::pre::save::findByIdAndUpdate::exit');
			doneAttaching();


		});

	});

	schema.pre('remove', function(doneRemoving) {
		var doc = this;
		var parentField = options.parentField;
		var parentModel = options.parentModel;
		var childCollection = options.childCollection;

		if (!doc[parentField]) return doneRemoving();
		

		var Parent = mongoose.model(parentModel);
		var pull = {};
		pull[childCollection] = doc._id;
		Parent.findByIdAndUpdate(doc[parentField], {
			$pull: pull
		}).exec(function(err) {
			
			if (err) {
				console.log('plugin::ParentAttachPlugin::schema::pre::save::findByIdAndUpdate::err', err);
				return doneRemoving(err);
			}

			console.log('plugin::ParentAttachPlugin::schema::pre::save::findByIdAndUpdate::exit');
			doneRemoving();


		});

	});

};

exports.parentAttach = ParentAttachPlugin;
exports.deleteChildren = DeleteChildrenPlugin;

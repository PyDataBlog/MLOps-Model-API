import React, { Component } from 'react';

export default React.createClass({
  getInitialState: function () {
    return { title: '', body: '' };
  },

  handleChangeTitle: function (e) {
    this.setState({ title: e.target.value });
  },

  handleChangeBody: function (e) {
    this.setState({ body: e.target.value });
  },

  handleSubmit: function (e) {
    e.preventDefault();
    this.props.addPost(this.state);

  },

  render() {
    return (
      <div>
        <h3>New post</h3>
        <form onSubmit={this.handleSubmit}>
          <input type="text"
            placeholder="sdfsd"
            value={this.title}
            placeholder="title"
            onChange={this.handleChangeTitle} />
          <br />
          <textarea type="text"
            placeholder="sdfsd"
            placeholder="body"
            onChange={this.handleChangeBody} >
            {this.body}
          </textarea>
          <button>Submit</button>
        </form>
      </div>
		);
  },
});

import React from 'react'

const EditableText = React.createClass({
  propTypes: {
    onSubmit: React.PropTypes.func.isRequired,
    validator: React.PropTypes.func,
    enableEditing: React.PropTypes.bool,
    value: React.PropTypes.oneOfType([
      React.PropTypes.string,
      React.PropTypes.number,
    ])
  },
  getInitialState: function() {
    return {
      editing: false,
      invalid: false,
      newValue: this.props.value,
    };
  },
  getDefaultProps: function() {
    return {
      enableEditing: true
    };
  },
  edit() {
    this.setState({editing: true});
  },
  handleChange(e) {
    this.setState({newValue: e.target.value});
  },
  cancelEdit() {
    this.setState({
      editing: false,
      invalid: false,
    });
  },
  submit(e) {
    e.preventDefault();
    if (!this.props.validator || this.props.validator(this.state.newValue)) {
      this.props.onSubmit(this.state.newValue);
      this.cancelEdit();
    } else {
      this.setState({invalid: true});
    }
  },
  renderInputForm() {
    const inputForm = (
      <form onSubmit={this.submit}
        className={this.state.invalid ? 'has-error' : ''} >
        <input type="text" autoFocus
        onChange={this.handleChange}
        onBlur={this.cancelEdit}
        value={this.state.newValue}
        className="form-control inline-editable" />
      </form>
    );
    return inputForm;
  },
  render: function() {
    if (this.props.enableEditing) {
      return (
        <div className="inline-edit">
          {this.state.editing
            ? this.renderInputForm()
            : <a onClick={this.edit} title="Edit" className={'inline-editable'}>{this.props.value || 'Add'}</a>
          }
        </div>
      );
    } else {
      return (<span>{this.props.value}</span>);
    }
  }

});

export default EditableText

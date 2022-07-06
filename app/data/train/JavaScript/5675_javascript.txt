import React, {PropTypes} from 'react';
import { Link, IndexLink } from 'react-router';
// import {Navbar, Nav, NavItem, NavDropdown, MenuItem} from 'react-bootstrap';

class Header extends React.Component {
  constructor(props) {
    super(props);
  }
  render() {
    return (
      <div id="container" className="link_box">
        <div id="itemA"></div>
        <div id="itemB">
          <Link to="/">
            <img src=" http://cdn.dscount.com/images_2016/top/ntop_all02.jpg" alt="dscount"/>
          </Link>
        </div>
        <div id="itemC">
          <Link to="/">
            <img src=" http://cdn.dscount.com/images_2016/top/btn_dscoupon02.jpg" alt="dscount"/>
          </Link>
        </div>
        <div id="itemD"></div>
        <div id="itemE">
          <Link to="" >ログイン</Link>
        </div>
        <div id="itemF">
          <Link to="/signup">会員登録</Link>
        </div>
        <div id="itemG">
          <a href="#" >マイページ{" "}▼</a>
        </div>
        <div id="itemH">
          <a href="#" >お客様センター{" "}▼</a>
        </div>
        <div id="itemI">
          <a href="#" >
            <span className="glyphicon glyphicon-shopping-cart">(0)</span>
          </a>
        </div>
        <div id="itemJ">
          <a href="#" >
            <span className="glyphicon glyphicon-heart">(1)</span>
          </a>
        </div>
        <form id="itemK">
          <div className="field">
            <button className="drop-down-btn" type="button" id="search">総合検索{" "}▼</button>
            <input className="header-search-input" type="text" id="searchterm" placeholder=" what do you want ?" />
            <button className="search-submit" type="button" id="search">
              <span className="glyphicon glyphicon-search"></span>
            </button>
          </div>
        </form>
        <div id="itemL"></div>
      </div>
    );
  }
}

export default Header;

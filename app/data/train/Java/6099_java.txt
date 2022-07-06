package com.cworld.earngold.sys.action;

import org.springframework.stereotype.Controller;

import com.core.code.action.BaseAction;
import com.cworld.earngold.mobile.inf.InterfaceLogin;
import com.cworld.earngold.mobile.inf.InterfaceUser;
import com.cworld.earngold.mobile.inf.impl.InterfaceLoginImpl;
import com.cworld.earngold.sys.beans.User;

@SuppressWarnings("serial")
@Controller
public class UserAction extends BaseAction {

	private String phone;

	private String pwd;
	
	private String checkPwd;

	private String checkCode;
	
	private String messageCode;

	private InterfaceUser infUser = null;

	private InterfaceLogin infLogin = null;

	private boolean isLogin() {
		if (this.httpSession.getAttribute("User") != null) {
			return true;
		} else {
			return false;
		}
	}

	public String login() {
		infLogin = new InterfaceLoginImpl();
		// User user = infLogin.login(phone, pwd);
		// Use to Test Start
		Integer count = (Integer) this.httpSession
				.getAttribute("PasswordFault");
		String checkCode = (String)this.httpSession.getAttribute("checkCode");
		System.out.println(count+"--------"+checkCode);
		if (null != count && count >= 3) {
			if(null == checkCode || checkCode.equalsIgnoreCase((String)this.httpSession.getAttribute("CheckCode"))){
				// Set Error Message
				return "loginPage";
			}
		}
		if ("admin".equalsIgnoreCase(phone) && "admin".equalsIgnoreCase(pwd)) {
			User user = new User();
			user.setUserId(1);
			user.setPhone("123456789");
			user.setType("admin");
			user.setAccountHead("123456789.png");
			// End
			if (user != null) {
				this.httpSession.setAttribute("user", user);
				if ("admin".equalsIgnoreCase(user.getType())) {
					System.out.println("adminPage");
					return "adminPage";
				} else {
					System.out.println("mainPage");

					return "mainPage";
				}
			} else {
				System.out.println("loginPage");
				passwordFaultFunc();
				return "loginPage";
			}
		} else {
			System.out.println("loginPage");
			passwordFaultFunc();
			return "loginPage";
		}
	}
	
	public String register(){
		return "";
	}

	public String getPhone() {
		return phone;
	}

	public void setPhone(String phone) {
		this.phone = phone;
	}

	public String getPwd() {
		return pwd;
	}

	public void setPwd(String pwd) {
		this.pwd = pwd;
	}

	private void passwordFaultFunc() {
		Integer count = (Integer) this.httpSession
				.getAttribute("PasswordFault");
		if (null == count) {
			this.httpSession.setAttribute("PasswordFault", 1);
		} else {
			this.httpSession.setAttribute("PasswordFault", count++);
		}
	}

	public String getCheckCode() {
		return checkCode;
	}

	public void setCheckCode(String checkCode) {
		this.checkCode = checkCode;
	}

	public String getCheckPwd() {
		return checkPwd;
	}

	public void setCheckPwd(String checkPwd) {
		this.checkPwd = checkPwd;
	}

	public String getMessageCode() {
		return messageCode;
	}

	public void setMessageCode(String messageCode) {
		this.messageCode = messageCode;
	}

}

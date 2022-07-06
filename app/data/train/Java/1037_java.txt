package com.xcode.bean;

import java.io.Serializable;

import javax.faces.bean.ManagedBean;
import javax.faces.bean.SessionScoped;

import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;

@ManagedBean
@SessionScoped
public class UserAuth implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 4027678448802658446L;
	
	private String email;

	public UserAuth() {
		SecurityContext context = SecurityContextHolder.getContext();
		Authentication auth = context.getAuthentication();		
		email = auth.getName();
	}
	
	public String getEmail() {
		return email;
	}
	
	public void setEmail(String email) {
		this.email = email;
	}
	
}

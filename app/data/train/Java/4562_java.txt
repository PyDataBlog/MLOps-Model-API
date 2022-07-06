package net.demo.model;

import javax.persistence.*;

/**
 * Created by toannh on 10/9/14.
 */
@Entity
@Table(name = "USER_ROLE")
public class UserRole {
  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private int id;
  private String username;
  private String role;

  @OneToOne(cascade = CascadeType.ALL)
  @JoinColumn(name = "username")
  private User user;

  public int getId() {
    return id;
  }

  public void setId(int id) {
    this.id = id;
  }

  public String getUsername() {
    return username;
  }

  public void setUsername(String username) {
    this.username = username;
  }

  public String getRole() {
    return role;
  }

  public void setRole(String role) {
    this.role = role;
  }

  public User getUser() {
    return user;
  }

  public void setUser(User user) {
    this.user = user;
  }
}

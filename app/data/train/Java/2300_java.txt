package com.jota.patterns.singleton;

import android.os.Bundle;
import android.support.v7.app.AppCompatActivity;
import android.widget.TextView;
import butterknife.BindView;
import butterknife.ButterKnife;
import butterknife.OnClick;
import com.jota.patterns.singleton.singleton.User;

public class MainActivity extends AppCompatActivity {

  @BindView(R.id.user1) TextView user1Text;
  @BindView(R.id.user2) TextView user2Text;
  @BindView(R.id.user3) TextView user3Text;

  private User user, user1, user2, user3;

  @Override protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    setContentView(R.layout.activity_main);
    ButterKnife.bind(this);

    user = User.getInstance();
    user.setToken("token");
    user.setSocialNetwork("Facebook");

    user1 = User.getInstance();
    user2 = User.getInstance();
    user3 = User.getInstance();
  }

  private void showUsers() {
    user1Text.setText(user1.getSocialNetwork() + " - " + user1.getToken());
    user2Text.setText(user2.getSocialNetwork() + " - " + user2.getToken());
    user3Text.setText(user3.getSocialNetwork() + " - " + user3.getToken());
  }

  @OnClick(R.id.change_social_button) public void changeSocial() {
    user.setSocialNetwork("Twitter");
    showUsers();
  }

  @OnClick(R.id.change_token_button) public void changeToken() {
    user.setToken("Token token");
    showUsers();
  }
}

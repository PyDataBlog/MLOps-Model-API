package com.example.myloading;

import android.app.Activity;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.ProgressBar;

public class MainActivity extends Activity {
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_main);
	}
	
	public void buttonMethod(View v){
		ProgressBar p = (ProgressBar)
				findViewById(R.id.progressBar1);
		p.setVisibility(View.VISIBLE); // 表示
	}
	
	public void buttonMethod2(View v){
		ProgressBar p = (ProgressBar)
				findViewById(R.id.progressBar1);
		p.setVisibility(View.GONE); // 非表示
	}
}

package me.banxi.androiddemo;
import android.app.Activity;
import android.os.Bundle;
import android.widget.TextView;

public class MainActivity extends Activity{
    public void onCreate(Bundle bundle){
        super.onCreate(bundle);
        TextView textView = new TextView(this);
        textView.setText("Hello,World");
        setContentView(textView);
    }
}

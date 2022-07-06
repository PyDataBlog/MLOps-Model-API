//package com.darktalker.cordova.webviewsetting;

package com.bluechatbox.android.displaysize;

import org.apache.cordova.CordovaPlugin;
import org.apache.cordova.CordovaWebView;
import org.apache.cordova.CordovaInterface;
import org.apache.cordova.CallbackContext;
import org.json.JSONArray;
import org.json.JSONException;

import android.os.Build;

import android.util.Log;

import android.view.Display;
import android.view.WindowManager;
import android.content.Context;

import android.graphics.Point;

public class DisplaySize extends CordovaPlugin {
    private CordovaWebView webView;
    private static final String LOG_TAG = "DisplaySize";
    
    @Override
    public void initialize(final CordovaInterface cordova, CordovaWebView webView) {
        this.webView = webView;
        super.initialize(cordova, webView); 
    }
    @Override
    public boolean execute(String action, JSONArray args, CallbackContext callbackContext) throws JSONException {
        
        if ("get".equals(action)) {

            Context ctx = this.cordova.getActivity().getApplicationContext(); 
            
            Display display = ((WindowManager) ctx.getSystemService(Context.WINDOW_SERVICE)).getDefaultDisplay();
            Point size = new Point();
            display.getSize(size);
            int width = size.x;
            int height = size.y;                    
            
            callbackContext.success(Integer.toString(width) + '-' + Integer.toString(height));
            return true;
        }
        
        
        return false;  // Returning false results in a "MethodNotFound" error.
        
    }
}

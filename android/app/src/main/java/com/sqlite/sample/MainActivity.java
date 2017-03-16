package com.sqlite.sample;

import android.app.Activity;
import android.content.Context;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.widget.TextView;

import java.io.*;

public class MainActivity extends Activity {

    private TextView tvOutput;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        tvOutput = (TextView) findViewById(R.id.tvOutput);
        test(this);
    }

    private Handler hOutput = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            tvOutput.append((String)msg.obj);
            super.handleMessage(msg);
        }
    };

    public void test(final Context ctx) {
        new Thread(new Runnable() {
            @Override
            public void run() {
                String dbPath = "/data/data/com.sqlite.sample/";
                String dbFile = dbPath + "demo.db";
                File fDb = new File(dbFile);
                if (!fDb.exists()) {
                    copyAssetFile(ctx, "demo.db", dbPath);
                }

                String out = "";
                boolean b = NativeAPI.dbOpen(dbFile);
                out += String.format("Open Database => %s\n", b ? "TRUE" : "FALSE");
                String error = NativeAPI.dbGetLastError();
                out += String.format("Open Database Error => %s\n", error);
                b = NativeAPI.dbSelect(dbFile, "select * from user");
                out += String.format("Select => %s\n", b ? "TRUE" : "FALSE");
                int count  = NativeAPI.dbGetSelectResultCount(dbFile);
                out += String.format("Select Rows => %d\n", count);

                for (int i = 0; i < count; i++) {
                    DemoRec r = NativeAPI.dbGetSelectResult(dbFile, i);
                    out += String.format("Data %d => {id => %d, name => %s}\n", i, r.AId, r.AName);
                }

                b = NativeAPI.dbClose(dbFile);
                out += String.format("Close Database => %s\n", b ? "TRUE" : "FALSE");
                Message msg = new Message();
                msg.obj = out;
                hOutput.sendMessage(msg);
            }
        }).start();
    }

    public boolean copyAssetFile(Context context, String fileName, String saveDir) {
        File fAsset = new File(saveDir);
        if (!fAsset.exists()) {
            fAsset.mkdirs();
        }
        try {
            byte[] buffer = new byte[8192];
            File dest = new File(saveDir + fileName);
            if (dest.exists()) {
                dest.delete();
            }
            InputStream is = context.getAssets().open(fileName);
            OutputStream fos = new BufferedOutputStream(new FileOutputStream(dest));

            int count = 0;
            int size = is.available();
            int n;
            while ((n = is.read(buffer, 0, buffer.length)) != -1) {
                fos.write(buffer, 0, n);
                count += n;
            }

            is.close();
            fos.close();
            return true;
        } catch (Exception ex) {
            return false;
        }
    }
}

package com.sqlite.sample;

public class NativeAPI {

    static {
        System.loadLibrary("sample");
    }

    public static native boolean dbOpen(String APath);
    public static native boolean dbClose(String APath);
    public static native boolean dbExecuteSQL(String APath, String ASQL);
    public static native boolean dbSelect(String APath, String ASQL);
    public static native int dbGetSelectResultCount(String APath);
    public static native DemoRec dbGetSelectResult(String APath, int AIndex);
    public static native String dbGetLastError();

}
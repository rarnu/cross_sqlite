//
//  NativeAPI.h
//  SQLiteDemo
//
//  Created by rarnu on 3/16/17.
//  Copyright © 2017 rarnu. All rights reserved.
//

#ifndef NativeAPI_h
#define NativeAPI_h

struct DemoRec {
    int AId;
    char* AName;
};

extern int dbOpen(const char* APath);
extern int dbClose(const char* APath);
extern int dbSelect(const char* APath, const char* ASQL);
extern int dbGetSelectResultCount(const char* APath);
extern struct DemoRec dbGetSelectResult(const char* APath, int AIndex);

#endif /* NativeAPI_h */

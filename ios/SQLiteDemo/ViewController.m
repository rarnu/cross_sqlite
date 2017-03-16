//
//  ViewController.m
//  SQLiteDemo
//
//  Created by rarnu on 3/16/17.
//  Copyright © 2017 rarnu. All rights reserved.
//

#import "ViewController.h"

@interface ViewController ()

@end

@implementation ViewController

- (void)viewDidLoad {
    [super viewDidLoad];
    
    NSString * originPath = [[NSBundle mainBundle] pathForResource:@"demo" ofType:@"db"];
    NSString * destPath = [ViewController getDocumentPath];
    NSString * dbFile = [destPath stringByAppendingPathComponent:@"demo.db"];
    [ViewController copyFile:originPath destFile:dbFile];
    
    int b = dbOpen([dbFile UTF8String]);
    printf("Open Database => %d\n", b);
    b = dbSelect([dbFile UTF8String], "select * from user");
    printf("Select => %d\n", b);
    int count = dbGetSelectResultCount([dbFile UTF8String]);
    printf("Select Rows => %d\n", count);
    for (int i = 0; i < count; i++) {
        struct DemoRec r = dbGetSelectResult([dbFile UTF8String], i);
        printf("Data %d => {id => %d, name => %s}\n", i, r.AId, r.AName);
    }
    
    b = dbClose([dbFile UTF8String]);
    printf("Close Database => %d\n", b);
}


- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

+(NSString *) getDocumentPath {
    NSArray * paths = NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSAllDomainsMask, YES);
    return paths[0];
}

+(BOOL) copyFile: (NSString *)sourceFile destFile: (NSString *)destFile {
    BOOL ret = NO;
    NSFileManager * fmgr = [NSFileManager defaultManager];
    ret = [fmgr copyItemAtPath:sourceFile toPath:destFile error:nil];
    return ret;
}

@end

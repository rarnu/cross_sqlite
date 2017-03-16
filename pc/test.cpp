#include <dlfcn.h>
#include <stdio.h>

struct DemoRec {
	int AId;
	char* AName;
};

typedef int (*dbOpen)(const char* APath);
typedef int (*dbClose)(const char* APath);
typedef int (*dbSelect)(const char* APath, const char* ASQL);
typedef int (*dbSelectResultCount)(const char* APath);
typedef struct DemoRec (*dbSelectResult)(const char* APath, int AIndex);

int main() {
	void* handle = dlopen("./libsample.so", RTLD_LAZY);
	dbOpen mOpen = (dbOpen) dlsym(handle, "dbOpen");
	dbClose mClose = (dbClose) dlsym(handle, "dbClose");
	dbSelect mSelect = (dbSelect) dlsym(handle, "dbSelect");
	dbSelectResultCount mSelectResultCount = (dbSelectResultCount) dlsym(handle, "dbGetSelectResultCount");
	dbSelectResult mSelectResult = (dbSelectResult) dlsym(handle, "dbGetSelectResult");

	const char* path = "demo.db";
	int b = mOpen(path);
	printf("Open Database => %d\n", b);
	b = mSelect(path, "select * from user");
	printf("Select => %d\n", b);
	int count = mSelectResultCount(path);
	printf("Select Rows => %d\n", count);
	for (int i = 0; i < count; i++) {
		DemoRec r = mSelectResult(path, i);
		printf("Data %d => {id => %d, name => %s}\n", i, r.AId, r.AName);
	}
	
	b = mClose(path);
	printf("Close Database => %d\n", b);
	return 0;
}

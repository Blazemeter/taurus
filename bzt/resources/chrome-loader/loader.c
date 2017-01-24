#include <stdio.h>
#include <unistd.h>
#include <windows.h>

#define JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE 0x00002000

HANDLE getJob(void);
char* getLog();
char* getVar(char *name);
char* getChromeParameters(int argc, char* argv[]);
void addToJob(HANDLE *job, PROCESS_INFORMATION *pi);
void createChildProcess(char* cmdline, STARTUPINFO *si, PROCESS_INFORMATION *pi);

// pointer of log file
FILE* FP;

 int main(int argc, char* argv[])
 {
	char *log_file_name, *chrome_path, *old_params, *new_cmdline, *new_params;

	// activate logging
	log_file_name = getLog();
	FP = fopen(log_file_name, "a");
	fprintf(FP, "Logging started\n");

	// read necessary variables
	chrome_path = getVar("PATH_TO_CHROME");
	new_params = getVar("ADDITIONAL_CHROME_PARAMS");
	fprintf(FP, "Envirionment variables read\n");

	// create job
	HANDLE ghJob = getJob();

	// build new cmdline
	fprintf(FP, "Path to real chrome: %s\n", chrome_path);
	old_params = getChromeParameters(argc, argv);
	new_cmdline = malloc(2048);
	snprintf(new_cmdline, 2047, "%s %s %s", chrome_path, new_params, old_params);

	fprintf(FP, "Old params: %s\n", old_params);
	fprintf(FP, "New params: %s\n", new_params);
		
	// create Chrome process
	STARTUPINFO si;
	PROCESS_INFORMATION pi;
	memset(&si, 0, sizeof(si));
	si.cb = sizeof(si);

	createChildProcess(new_cmdline, &si, &pi);

	// add Chrome to job
	addToJob(&ghJob, &pi);

	fclose(FP);

	//wait till the Chrome process ends
	WaitForSingleObject(pi.hProcess, INFINITE);

	return 0;
}

void createChildProcess(char* cmdline, STARTUPINFO *si, PROCESS_INFORMATION *pi)
{
	fprintf(FP, "Trying to create child process: %s\n", cmdline);
	if(!CreateProcess(NULL, cmdline, NULL, NULL, FALSE,
		NORMAL_PRIORITY_CLASS | CREATE_BREAKAWAY_FROM_JOB, NULL, NULL, si, pi))
	{
		fprintf(FP, "Could not create child process, errorcode: %d\n", GetLastError());
		exit(1);
    }
	fprintf(FP, "Child process created\n");
}

char* getChromeParameters(int argc, char* argv[])
{
	char* cmdline = malloc(2048);
	strcpy(cmdline, " ");

	// concatenate arguments, skip the program name
	for (int i = 1; i < argc; i++)
	{
		strcat(cmdline, argv[i]);
		strcat(cmdline, " ");
	}
	return cmdline;
}

void addToJob(HANDLE *job, PROCESS_INFORMATION *pi)
{
	if(0 == AssignProcessToJobObject(*job, pi->hProcess))
	{
		fprintf(FP, "Could not assing Chrome process to job, errorcode: %d\n", GetLastError());
		exit(1);
	}
	fprintf(FP, "Chrome process assigned to job\n");
}

HANDLE getJob(void)
{
	HANDLE job = CreateJobObject(NULL, NULL);
	if(job)
	{
		fprintf(FP, "Job created\n");
		JOBOBJECT_EXTENDED_LIMIT_INFORMATION jeli = {0};
		jeli.BasicLimitInformation.LimitFlags =
			JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE | JOB_OBJECT_LIMIT_BREAKAWAY_OK;

		if(!SetInformationJobObject(job, JobObjectExtendedLimitInformation, &jeli, sizeof(jeli)))
		{
			fprintf(FP, "Could not configure job, errorcode: %d\n", GetLastError());
			exit(1);
		}
		fprintf(FP, "Job confugured\n");
		return job;
	}
	else
	{
		fprintf(FP, "Could not create job object, errorcode: %d\n", GetLastError());
		exit(1);
	}
}

char* getVar(char *name)
{
	char* val;
	val	= getenv(name);
	if ((val) && ((*val)))
		return val;
	else
	{
		fprintf(FP, "You must set environment variable: %s\n", name);
		exit(1);
	}
}

char* getLog()
{
	char* val;
	char* name = "CHROME_LOADER_LOG";

	val	= getenv(name);
	if ((val) && ((*val)))
	{
		printf("chrome-loader log file: %s\n", name);
		return val;
	}
	else
	{
		printf("You must set environment variable: %s\n", name);
		exit(1);
	}
}
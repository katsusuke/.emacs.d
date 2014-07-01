var EMACS_HOME = "C:\\Program Files (x86)\\Emacs";
var EMACSCLIENT = "\"" + EMACS_HOME + "\\bin\\emacsclient" + "\"";
var RUNEMACS = EMACS_HOME + "\\bin\\runemacs";
var EMACS_EXE = EMACS_HOME + "\\bin\\emacs.exe";

function GetPCName(){
    var objNetWork = new ActiveXObject("WScript.Network");
    var res = objNetWork.ComputerName;
    objNetWork = null
    return res;
}

var PC_NAME = GetPCName();
var shell = WScript.CreateObject("WScript.Shell");
var appDataDir = shell.ExpandEnvironmentStrings("%APPDATA%");
var serverFile = appDataDir + "\\.emacs.d\\server\\server";

//WScript.Echo("serverFile:"+ serverFile);
var fso = WScript.CreateObject("Scripting.FileSystemObject");

var args = "";
for(var i = 0; i < WScript.Arguments.length; ++i){
    if(i != 0){
	args += " ";
    }
    args += '"' + WScript.Arguments(i) + '"';
}

function getEmacsPID(){
    var wmiObj = GetObject("WinMgmts:Root\\Cimv2");
    var processes = wmiObj.ExecQuery("Select * From Win32_Process");
    var processenum = new Enumerator(processes);
    var flag = 0;
    var pid = null;
//    WScript.Echo(EMACS_EXE.toLowerCase());
    for(; !processenum.atEnd(); processenum.moveNext()){
	var item = processenum.item();
	if(item.CommandLine != null){
//	    WScript.Echo("cmd:" + item.CommandLine.toLowerCase() + " index:" + item.CommandLine.toLowerCase().indexOf(EMACS_EXE.toLowerCase()));
	    if(item.CommandLine.toLowerCase().indexOf(EMACS_EXE.toLowerCase()) !== -1){
		pid = item.ProcessId;
		break;
	    }
	}
    }
    return pid;
}

var pid = getEmacsPID();
//WScript.Echo(pid);
if(pid === null && fso.FileExists(serverFile)){
    fso.DeleteFile(serverFile);
}

if(pid === null){
    shell.Exec(RUNEMACS);
}

while(!fso.FileExists(serverFile)){
    WScript.Sleep(1);
}

//WScript.Echo(EMACSCLIENT + " " + args);
shell.Run(EMACSCLIENT + " " + args, 0);
shell.AppActivate("emacs@" + PC_NAME);

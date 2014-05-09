var EMACS_HOME = "C:\\Program Files (x86)\\Emacs";
var EMACSCLIENT = "\"" + EMACS_HOME + "\\bin\\emacsclient" + "\"";
var RUNEMACS = EMACS_HOME + "\\bin\\runemacs";

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

//WScript.Echo("exist:" + fso.FileExists(serverFile));

if(!fso.FileExists(serverFile)){
    shell.Exec(RUNEMACS);
}

while(!fso.FileExists(serverFile)){
    WScript.Sleep(1);
}


//WScript.Echo(EMACSCLIENT + " " + args);
shell.Run(EMACSCLIENT + " " + args, 0);
shell.AppActivate("emacs@" + PC_NAME);

set path=%path%;E:/x64unzip/Git/2.6.4/bin/;E:/x64editor/Inno Setup 5;
set SCRIPT_DIR=%~dp0
java -Xss1M -XX:+CMSClassUnloadingEnabled -XX:-OmitStackTraceInFastThrow -Dsbt.override.build.repos=true -Dsbt.ivy.home=e:/pro/baiduyun/x32editor/sbtIvy -jar "%SCRIPT_DIR%/project/sbt-launch-0.13.7.jar" %*
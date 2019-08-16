#/bin/bash
# This script will install Metalibm on your computer

confirm=""
existing_inst=$METALIBM_PATH # (env | grep -q "^METALIBM_PATH=") # existing_inst=${tmp#"METALIBM_PATH="}


if [[ -z $confirm && ! -z $existing_inst ]]; then
	echo "Metalibm is already installed. Do you want to install it again? (y/n)"
	read confirm
	if [[ $confirm != "y" ]]; then
		echo "Installation cancelled."
		exit 0
	fi
fi

# detect current OS
os=""
if [[ $OSTYPE == "darwin"* ]]; then
	os="macOS"
elif [[ $OSTYPE == "linux-gnu" ]]; then
  	os="linux"
else 
	echo "Installation is not supported for your OS ($OSTYPE)\n"
	exit 1
fi
echo "Installing Metalibm on $os..."

# make sure gcc is not linked to clang (o.w. warn to remove the symlink)
gcc_=$(gcc --version | grep "gcc")
if [[ -z gcc_ ]]; then
	echo "Cannot find gcc. It can be symlinked to clang, remove the symlink and rerun the script."
	exit 1
fi

# make sure g++ is not linked to clang (o.w. warn to remove the symlink)
gplpl=$(g++ --version | grep "g++")
if [[ -z gplpl ]]; then
	echo "Cannot find g++. It can be symlinked to clang, remove the symlink and rerun the script."
	exit 1
fi

# for macOS make sure sed is GNU sed
if [[ os -eq "macOS" ]]; then
    sedv=$(sed --version | grep "GNU")
    if [[ -z sedv ]]; then
        echo "Looks like you do not have GNU sed installed. (Probably only default Mac sed)."
        echo "Try running: "
        echo "$ brew install gnu-sed --with-default-names"
        echo ""
        echo "If you already installed GNU sed, but it is not found, try creating symlink (gnu-sed --> sed)."
        exit 1
    fi
fi

# Find Daisy home directory

IFS='/ ' read -r -a currTree <<< $(pwd)
path=""
inDaisy=false
for i in "${!currTree[@]}"; do 
	path="$path/${currTree[$i]}"
	if [[ "${currTree[$i]}" == "daisy" ]]; then
		inDaisy=true
		break
	fi
done
echo $path

if [ $inDaisy = false ] ; then
	echo "Please run the script from one of the Daisy's directories. Installation is not finished!"
	exit 1
fi

metalibmdir="${path}/metalibm"
if [[ ! -d "$metalibmdir" ]]; then
	echo "Metalibm source files are not found in Daisy's home directory. Please place them here:"
	echo $metalibmdir
	exit 1
fi

cd metalibmdir
export METALIBM_PATH=$metalibmdir
export PATH=$PATH:$metalibmdir

echo "Trying to build Metalibm..."
# also add it to PATH
esc_metalibmdir=$(printf '%s\n' "$metalibmdir" | sed 's:[\/&]:\\&:g;$!s/$/\\/')
# TODO do not add metalibmdir to the path if it's already there
 if [[ $os == "macOS" ]]; then
 	# check macOS version
 	vstr=$(sw_vers -productVersion)
 	IFS='. ' read -r -a version <<< "$vstr"
 	if [[ ${version[0]} -eq 10 && ${version[1]} -gt 13 ]]; then
 		if [[ ! $LIBRARY_PATH == *"/usr/local/lib"* ]]; then
 			esc_lib=$(printf '%s\n' "/usr/local/lib" | sed 's:[\/&]:\\&:g;$!s/$/\\/')
 			sed -i -e "\$aexport LIBRARY_PATH=\$LIBRARY_PATH:$esc_lib" ~/.bash_profile
 		fi

 		if [[ ! $LIBRARY_PATH == *"/usr/local/include"*  ]]; then
 			esc_lib=$(printf '%s\n' "/usr/local/include" | sed 's:[\/&]:\\&:g;$!s/$/\\/')
 			sed -i -e "\$aexport LIBRARY_PATH=\$LIBRARY_PATH:$esc_lib" ~/.bash_profile
 		fi
 		echo "macOS version $vstr has additional requirements for the installation."
 		echo "Check that LIBRARY_PATH includes path to MPFR, GMP and Sollya"
 		echo "$LIBRARY_PATH"
 		echo "Continue? (y/n)"
 		read libcheck
 		if [[ "$libcheck" != "y" ]]; then
 			echo "Interrupted the installation"
 			exit 0
 		fi
 	fi

	# remove static flags from all Makefiles
	for f in $(find . -name "Makefile") 
	do
		sed -i -e "s/-static//g" $f
	done
	make

 	# add var METALIBM_PATH to the system environment with path to metalibm
 	# for macOS edit ~/.bash_profile
 	if [[ ! -z existing_inst ]]; then
 		sed -i -e "/export METALIBM_PATH/d" ~/.bash_profile
 	fi
 	echo "export METALIBM_PATH=$metalibmdir" >> ~/.bash_profile
 	# also add it to PATH
 	sed -i -e "\$aexport PATH=\$PATH:$esc_metalibmdir" ~/.bash_profile
 	source ~/.bash_profile
 else # linux
 	make
 	# add var METALIBM_PATH to the system environment with path to metalibm
 	# for linux edit ~/.bashrc  # or /etc/environment:  sudo -H /etc/environment >> VARNAME="my value"
 	if [[ -z existing_inst ]]; then
 		sed -i -e "/^export METALIBM_PATH=/d" ~/.bashrc
 	fi
 	echo "export METALIBM_PATH=$metalibmdir" >> ~/.bashrc
 	# also add it to PATH
 	sed -i -e "\$aexport PATH=\$PATH:$esc_metalibmdir" ~/.bashrc
 	source ~/.bashrc
 fi

# modify metalibm.sollya & execute statements in *.sollya to use abs path
echo "Integrating Metalibm with Daisy ..."
echo " - run outside of home directory"

sed -i -e "s/^__metalibmdir =.*/__metalibmdir = \"$esc_metalibmdir\";/g" metalibm.sollya # replace command returning metalibm path by its absolute path
sed -i -e "s/execute(\"config.sollya\");/execute(__metalibmdir @ \"config.sollya\");/g" metalibm.sollya
sed -i -e "s/execute(\"implement.sollya\");/execute(__metalibmdir @ \"implement.sollya\");/g" metalibm.sollya
sed -i -e "s/compilePath = \"\";/compilePath = \"$esc_metalibmdir\";/g" config.sollya

# put all output files in folder metalibm is executed from
echo " - define path for output files - dir Metalibm is executed from"
sed -i "s/implementationFile = __metalibmdir @ \"\/\" @ implementationFile;/implementationFile = bashevaluate(\"pwd\") @ \"\/\" @ implementationFile;/g" metalibm.sollya
sed -i "s/gappaFile = __metalibmdir @ \"\/\" @ gappaFile;/gappaFile = bashevaluate(\"pwd\") @ \"\/\" @ gappaFile;/g" metalibm.sollya
sed -i "s/gappaOutputTmpFile = __metalibmdir @ \"\/\" @ gappaOutputTmpFile;/gappaOutputTmpFile = bashevaluate(\"pwd\") @ \"\/\" @ gappaOutputTmpFile;/g" metalibm.sollya
sed -i "s/compiledCode = __metalibmdir @ \"\/\" @ compiledCode;/compiledCode = bashevaluate(\"pwd\") @ \"\/\" @ compiledCode;/g" metalibm.sollya

echo " - disable recompilation and plotting"
# disable recompiling before each execution
sed -i "/Recompiling/,/Metalibm ready to run/d" metalibm.sollya
# modify such that Metalibm does not do plotting 
sed -i -e "s/doplots = true;/doplots = false;/g" metalibm.sollya
sed -i -e "s/dotiming = true;/dotiming = false;/g" metalibm.sollya
# disable compiling resulting polynomial
sed -i "/bashexecute(compilepath@\"compile.sh \"@compilepath@\"/d" metalibm.sollya
sed -i "/bashexecute(\"rm \" @ compiledCode);/d" metalibm.sollya

echo " - add necessary output info"
# add information in the end
sed -i "/quit/d" metalibm.sollya 
echo "write(\"\nfunctionName = \", resultat.functionname, \" implErr = \", resultat.implerr);" >> metalibm.sollya
echo "quit;" >> metalibm.sollya

# modify implement.sollya to not use argument reduction
# delete lines that execute implementation with argument reduction
echo " - disable argument reduction"
sed -i -e "9256,9297d" implement.sollya 
sed -i "9256i \ \ \ \ \ tryImplementationChoices = [| { .method = tryImplementationThroughDomainSplitting, .name = \"tryImplementationThroughDomainSplitting\" } |];" implement.sollya

# make implementation names shorter
echo " - make implementation name shorter"
sed -i -e "s/implementationFile = \"implementation.c\";/implementationFile = \"impl.c\";/g" config.sollya
sed -i -e "s/gappaFile = \"implementation.gappa\";/gappaFile = \"impl.gappa\";/g" config.sollya

# warn about rebooting
echo "Logout and login or reboot for changes to be applied"

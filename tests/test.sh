#!/bin/bash

RED='\e[0;31m'
GREEN='\e[0;32m'
NC='\e[0m'


DEBUG=0
while getopts ":d" opt; do
  case $opt in
    d)
      DEBUG=1
      ;;
    \?)
      echo "Invalid option: -$OPTARG" ;;
  esac
done


cp ../joosc . && cp ../compiler.jar .
if (( $? != 0 ));
then
  echo -e "${RED}Generate compiler jar first${NC}"
  exit 1
fi

TEST_DIRS=`find ../compiler/src/test/resources -type d -name valid | grep integ` # integ only for now
STD_LIB=`find ../compiler/src/test/resources/integ/stdlib -type f`

num_tests=0
passed_tests=0

for dir in ${TEST_DIRS}
do
  for test_case in `find ${dir} -maxdepth 1 -type d -not -path "*/valid"`
  do
    echo "Running ${test_case}..." | sed 's/\.\..*\/\(a[0-9]\|integ\)\/.*valid\/\(.*\)/\1 \2/g'
    (( num_tests += 1 ))

    rm -rf output
    mkdir output
    cp ../output/runtime.s output
    
    files=`find ${test_case} -type f`
    ./joosc ${files} ${STD_LIB}
    if (( $? != 0 )) 
    then
      echo -e "${RED}FAILED at Code Generation...${NC}"
      continue
    fi

    flag=0
    for asm in `find output -type f`
    do
      nasm -O1 -f elf -g -F dwarf ${asm}
      if (( $? != 0 ))
      then
        flag=1
      fi
    done

    if (( flag == 1 )) 
    then
      echo -e "${RED}FAILED at Assembler...${NC}"
      continue
    fi

    ld -melf_i386 -o main output/*.o
    if (( $? != 0 ))
    then
      echo -e "${RED}FAILED at Linker...${NC}"
      continue
    fi

    ./main
    result=$?
    if (( result != 123 && result != 13 ))
    then
      echo -e "${RED}FAILED at Execution... (Return value was ${result}${NC}"
      read -p "Press [Enter] to continue..."
    else
      echo -e "${GREEN}PASSED!${NC} (Returned ${result})"
      (( passed_tests += 1 ))
    fi
    

    if (( DEBUG == 1 ))
    then
      read -p "DEBUG MODE ON :: Press [Enter] to continue..."
    fi
  done
done

(( failed_tests= num_tests - passed_tests ))

echo -e "Results: ${GREEN}${passed_tests}${NC} of ${num_tests} ${GREEN}passed${NC}. (${RED}${failed_tests}${NC} failed)"



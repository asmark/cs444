#!/bin/bash

RED='\e[0;31m'
GREEN='\e[0;32m'
NC='\e[0m'

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
    echo "Running ${test_case}" | sed 's/\.\..*\/\(a[0-9]\|integ\)\/.*valid\/\(.*\)/\1 \2/g'
    (( num_tests += 1 ))

    rm -rf output
    mkdir output
    cp ../output/runtime.s output
    
    files=`find ${test_case} -type f`
    ./joosc ${files} ${STD_LIB}
    if (( $? != 0 )) 
    then
      echo -e "${RED}FAILED at Code Generation...${NC}"
      break
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
      break
    fi

    ld -melf_i386 -o main output/*.o
    if (( $? != 0 ))
    then
      echo -e "${RED}FAILED at Linker...${NC}"
      break
    fi

    ./main
    if (( $? != 123 && $? != 13 ))
    then
      echo -e "${RED}FAILED at Execution...${NC}"
    else
      echo -e "${GREEN}PASSED!${NC}"
      (( passed_tests += 1 ))
    fi
  done
done

(( failed_tests= num_tests - passed_tests ))

echo -e "Results: ${GREEN}${passed_tests}${NC} of ${num_tests} ${GREEN}passed${NC}. (${RED}${failed_tests}${NC} failed)"



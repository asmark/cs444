#!/bin/bash

RED='\e[0;31m'
GREEN='\e[0;32m'
BLUE='\e[0;34m'
NC='\e[0m'

TEST_FILTER=".*"
if (( $# >= 1 ))
then
  TEST_FILTER="$@"
fi

DEBUG=0

cp ../joosc . && cp ../compiler.jar .
if (( $? != 0 ));
then
  echo -e "${RED}Generate compiler jar first${NC}"
  exit 1
fi

TEST_DIRS=`find ../compiler/src/test/resources -regextype posix-egrep -regex ".*/(valid|exception)" -type d | grep "/a5/"`
STD_LIB=`find ../compiler/src/test/resources/integ/stdlib -type f`
FILE_FAILED_TESTS=failed-tests.txt

num_tests=0
passed_tests=0

rm -f ${FILE_FAILED_TESTS}

for dir in ${TEST_DIRS}
do
  for test_case in `find ${dir} -maxdepth 1 -type d -not -path "*/valid" -not -path "*/exception" | grep -i "${TEST_FILTER}"`
  do
    test_name=`echo ${test_case} | sed 's/\.\..*\/\(a[0-9]\|integ\)\/.*\(exception\|valid\)\/\(.*\)/\1 \3/g' | sed 's/ /-/g'`
    echo "Running ${test_name}..."
    (( num_tests += 1 ))

    outdir=`echo "${test_name}"`

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
  
    rm -rf ${outdir}
    mv output ${outdir}

    flag=0
    for asm in `find ${outdir} -type f`
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

    ld -melf_i386 -o ${outdir}/main ${outdir}/*.o
    if (( $? != 0 ))
    then
      echo -e "${RED}FAILED at Linker...${NC}"
      continue
    fi

    ./${outdir}/main
    result=$?

    if [[ ${test_case} == *exception* ]]
    then
      expected=13
    else
      expected=123
    fi

    if (( result != expected ))
    then
      echo "${test_name}" >> ${FILE_FAILED_TESTS}
      echo -e "${RED}FAILED at Execution... (Return value was ${result}${NC}"
    else
      if (( result == 123 )) 
      then
        echo -e "${GREEN}PASSED!${NC} (Returned ${result})"
      else
        echo -e "${BLUE}PASSED!${NC} Threw an exception. (Returned ${result})"
      fi
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



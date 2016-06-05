passed=0
failed=0

for f in $@
do
        $f >/dev/null 2>/dev/null
        res=$?
        if [[ $res == 0 ]]
        then
                echo -e "\e[32mOK:\e[39m   $f"
                passed=$(expr $passed + 1)
        else
                echo -e "\e[31mFAIL:\e[39m $f"
                failed=$(expr $failed + 1)
        fi
done

if [[ $failed == 0 ]]
then
        echo -e "\e[32m$passed succeeded\e[39m"
else
        echo -e "\e[31m$failed failed, $passed succeeded\e[39m"
fi
exit $failed

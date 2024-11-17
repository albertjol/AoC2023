#include <stdio.h>
#include <stdint.h>
#include <unistd.h>

int main()
{
    char ch;
    int curValue = 0;
    int result = 0;
    while(read(STDIN_FILENO, &ch, 1) > 0)
    {
        if(ch == 10 || ch == 13)
            continue;
        if(ch == ',')
        {
            result += curValue;
            curValue = 0;
            continue;
        }

        curValue += ch;
        curValue *= 17;
        curValue &= 0xFF;
    }
    result += curValue;

    printf("Value = %d", result);
    return 0;
}
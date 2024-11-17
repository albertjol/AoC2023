#include <stdio.h>
#include <stdint.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>

typedef struct
{
    char type[10];
    int focalLength;
} Lens;

uint8_t hash(char* type)
{
    int i = 0;
    int result = 0;
    while(type[i] > 0)
    {
        result += type[i];
        result *= 17;
        result &= 0xFF;
        i++;
    }
    return (uint8_t)result;
}

int main()
{
    char ch;
    char type[10];
    memset(type, 0, sizeof(type));
    int step = 0;
    int64_t result = 0;
    int i = 0, j = 0;
    int index;
    Lens boxes[256][256];
    memset(boxes, 0, sizeof(boxes));
    while(read(STDIN_FILENO, &ch, 1) > 0)
    {
        if(ch == 10 || ch == 13)
            continue;
        if(ch == ',')
        {
            i = 0;
            step = 0;
            memset(type, 0, sizeof(type));
            continue;
        }
        switch (step)
        {
            case 0: // type
                if(ch == '=') 
                    step = 1;
                else if(ch == '-')
                {
                    index = hash(type);
                    int deleted = 0;
                    for(j = 0; j < 256; ++j)
                    {
                        if (!strcmp(boxes[index][j].type, type))
                        {
                            deleted = 1;
                        }
                        else if (deleted)
                        {
                            boxes[index][j-1] = boxes[index][j];
                            if(boxes[index][j].focalLength == 0)
                                break;
                        }
                    }
                }
                else
                {
                    type[i++] = ch;
                }
                break;
            case 1: //focalLength
                index = hash(type);

                for(j = 0; j < 256; ++j)
                {
                    if (!strcmp(boxes[index][j].type, type))
                    {
                        boxes[index][j].focalLength = ch - 48;
                        break;
                    }
                    else if (boxes[index][j].focalLength == 0)
                    {
                        strcpy(boxes[index][j].type, type);
                        boxes[index][j].focalLength = ch - 48;
                        break;
                    }
                }
                break;
        }
    }

    int power;
    for(i = 0; i < 256; ++i)
    {
//        printf("Box %d : ", i);
        for(j = 0; j < 256; ++j)
        {
            if (boxes[i][j].focalLength == 0)
            {
//                puts("");
                break;
            }
            power = ((i+1)*(j+1)*(boxes[i][j].focalLength));
            result += power;
//            printf("[%s %d -> %d] ", boxes[i][j].type, boxes[i][j].focalLength, power);
        }            
    }

    printf("Value = %d", result);
    return 0;
}
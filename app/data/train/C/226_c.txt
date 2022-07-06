//
//  main.c
//  demo14
//
//  Created by weichen on 15/1/9.
//  Copyright (c) 2015年 weichen. All rights reserved.
//

#include <stdio.h>

int main() {
    // 求输入的数的平均数，并输出大于平均数的数字
    
    int x = 0;      //输入的数
    double num = 0; //总和（这个定义为double, 因为计算结果可能出现浮点数）
    int count = 0;  //个数
    double per;     //平均数（结果也定义double）
    
    printf("请输入一些数:");
    
    scanf("%d", &x);
    
    //不等于0时执行累计；输入等于0的值,来终止循环，并执行后面的代码
    while(x != 0)
    {
        num += x;
        count++;
        
        scanf("%d", &x);
    }
    
    
    if(count > 0)
    {
        per = num / count;
    
        printf("%f \n", per);
    }
    
    return 0;
}

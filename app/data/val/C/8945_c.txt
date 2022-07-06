//
// Created by Tomas Linhart.
// Copyright (c) 2014 Tomáš Linhart. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <UIKit/UIKit.h>

typedef NS_ENUM(NSInteger, TLLinearLayoutViewOrientation) {
    TLLinearLayoutViewOrientationVertical,
    TLLinearLayoutViewOrientationHorizontal,
};

@interface TLLinearLayoutView : UIView

@property (nonatomic, assign) TLLinearLayoutViewOrientation orientation;

- (instancetype)initWithOrientation:(TLLinearLayoutViewOrientation)orientation;

@end
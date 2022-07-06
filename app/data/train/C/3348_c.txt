//
//  RestaurantGuard.h
//  demoProtocol
//
//  Created by techmaster on 9/11/13.
//  Copyright (c) 2013 Techmaster. All rights reserved.
//

#import <Foundation/Foundation.h>

@protocol RestaurantGuard <NSObject>
@required
- (BOOL) isHeightOver18;
- (void) speakEnglish;

@optional
- (void) speakJapanese;
@end

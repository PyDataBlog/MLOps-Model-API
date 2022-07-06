//
//  Card.h
//  Matchismo
//
//  Created by Horia Jurcut on 01/07/16.
//  Copyright © 2016 Horia Jurcut. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface Card : NSObject

@property (strong, nonatomic) NSString *contents;
@property (nonatomic, getter=isChosen) BOOL chosen;
@property (nonatomic, getter=isMatched) BOOL matched;

- (int)match:(NSArray *)otherCards;

@end

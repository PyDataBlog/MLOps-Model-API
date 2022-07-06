//
//  PlayingCard.h
//  Matchismo
//
//  Created by Horia Jurcut on 02/07/16.
//  Copyright © 2016 Horia Jurcut. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "Card.h"

@interface PlayingCard : Card

@property (strong, nonatomic) NSString *suit;
@property (nonatomic) NSUInteger rank;

+ (NSArray *)validSuits;
+ (NSUInteger)maxRank;

@end

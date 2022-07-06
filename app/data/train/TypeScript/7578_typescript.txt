module TSXKP {
    export module Configs {

        export enum SeparatorCharacterType {
            None            = 0,
            Random          = 1,
            Specific        = 2
        }

        export enum PaddingType {
            None            = 0,
            Fixed           = 1,
            Adaptive        = 2
        }

        export enum PaddingCharacterType {
            None            = 0,
            Random          = 1,
            Separator       = 2,
            Specific        = 3
        }

        export enum CaseTransform {
            None            = 0,
            Upper           = 1,
            Lower           = 2,
            Capitalise      = 3,
            Invert          = 4,
            Alternate       = 5,
            Random          = 6
        }

        export enum RandomIncrement {
            Auto            = 0,
            Specific        = 1
        }

        export interface IRandomGenerator {
            (count: number): number[];
        }

        export var RandomGenerator: IRandomGenerator = function (count: number): number[] {
            var numbers: number[];
            var i: number;
            numbers = [];

            for (i = 0; i < count; ++i) {
                numbers.push(Math.random());
            }

            return numbers;
        }

        export interface CharacterSubstitutions {
            [key: string]: string;
        }

        export interface IConfig {
            symbol_alphabet: string[];
            separator_alphabet: string[];
            word_length_min: number;
            word_length_max: number;
            num_words: number;
            separator_character_type: SeparatorCharacterType;
            separator_character?: string;
            padding_digits_before: number;
            padding_digits_after: number;
            padding_type: PaddingType;
            padding_character_type: PaddingCharacterType;
            padding_character?: string;
            padding_characters_before?: number;
            padding_characters_after?: number;
            pad_to_length?: number;
            case_transform: CaseTransform;
            character_substitutions: CharacterSubstitutions;
        }
    }
}
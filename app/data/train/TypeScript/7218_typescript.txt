import getMatchedSockCount, { PairSocks } from '../sock-merchant';

describe('SockMerchant', () => {
    describe('PairSocks class', () => {
        it('default constructor', () => {
            let socks = new PairSocks();

            expect(socks.unmatchedCount).toEqual(0);
        });

        it('add one unmatched sock, and count should be 1.', () => {
            let socks = new PairSocks();
            socks.addSock(50);

            expect(socks.unmatchedCount).toEqual(1);
            expect(socks.isUnmatched(50)).toEqual(true);
        });

        it('sock match should set count down to 0.', () => {
            let socks = new PairSocks();
            socks.addSock(50);
            socks.addSock(50);

            expect(socks.unmatchedCount).toEqual(0);
            expect(socks.isUnmatched(50)).toEqual(false);
        });

        it('add multiple unmatched socks.', () => {
            let socks = new PairSocks();

            socks.addSock(6);
            socks.addSock(5);
            expect(socks.unmatchedCount).toEqual(2);

            socks.addSock(2);
            expect(socks.unmatchedCount).toEqual(3);

            socks.addSock(3);
            expect(socks.unmatchedCount).toEqual(4);

            socks.addSock(5);
            expect(socks.unmatchedCount).toEqual(3);
        });
    });

    describe('test actual inputs.', () => {
        it('Sample Input on Q', () => {
            expect(getMatchedSockCount([10, 20, 20, 10, 10, 30, 50, 10, 20])).toEqual(3);
        });

        describe('Test Case #2', () => {
            it('1/3', () => {
                expect(getMatchedSockCount([6, 5, 2, 3, 5])).toEqual(1);
            });

            it('2/3', () => {
                expect(getMatchedSockCount([2, 2, 1, 1, 5])).toEqual(2);
            });

            it('3/3', () => {
                expect(getMatchedSockCount([1, 3, 3, 3, 5])).toEqual(1);
            });

            it('1/2', () => {
                expect(getMatchedSockCount([6, 5, 2, 3, 5, 2, 2, 1])).toEqual(2);
            });

            it('1/2', () => {
                expect(getMatchedSockCount([6, 5, 2, 3, 5, 2, 2, 1])).toEqual(2);
            });

            it('2/2', () => {
                expect(getMatchedSockCount([1, 5, 1, 3, 3, 3, 5])).toEqual(3);
            });

            it('Complete', () => {
                expect(getMatchedSockCount([6, 5, 2, 3, 5, 2, 2, 1, 1, 5, 1, 3, 3, 3, 5])).toEqual(6);
            });
        });
    });
});
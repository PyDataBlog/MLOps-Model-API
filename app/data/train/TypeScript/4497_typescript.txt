import { PlayService } from './play.service';

describe('Play Service', () => {

  let playService: PlayService;
  let mockedSeed: any;

  beforeAll(() => {
    playService = new PlayService();
    mockedSeed = [
      [
        {
          coordinates: {x: 0, y: 0},
          isAlive: false
        },
        {
          coordinates: {x: 0, y: 1},
          isAlive: true
        },
        {
          coordinates: {x: 0, y: 2},
          isAlive: false
        }
      ],
      [
        {
          coordinates: {x: 1, y: 0},
          isAlive: false
        },
        {
          coordinates: {x: 1, y: 1},
          isAlive: true
        },
        {
          coordinates: {x: 1, y: 2},
          isAlive: false
        }
      ],
      [
        {
          coordinates: {x: 2, y: 0},
          isAlive: false
        },
        {
          coordinates: {x: 2, y: 1},
          isAlive: true
        },
        {
          coordinates: {x: 2, y: 2},
          isAlive: false
        }
      ]
    ];
  });

  it('should initialize the seed to defined value', () => {
    expect(playService.getSeed()).toBeDefined();
  });

  it('should initialize the prev gen seed to undefined value', () => {
    expect(playService.getPreviousGenSeed()).toBeUndefined();
  });

  it('should initialize ticker to undefined value', () => {
    expect(playService.ticker).toBeUndefined();
  });

  it('should initialize tickerspeed to 500 value', () => {
    expect(playService.getTickSpeed()).toEqual(500);
  });

  it('should follow Conway\'s Game Of Rules', () => {
    playService.setSeed(mockedSeed);
    playService.setPreviousGenSeed(mockedSeed);
    playService.processNextGen();
    playService.getSeed().forEach(row => {
      row.forEach(cell => {
        if ((cell.coordinates.y >= 0 && cell.coordinates.y <= 2) && cell.coordinates.x === 1) {
          expect(cell.isAlive).toBeTruthy();
        }
      });
    });
  });

});

export class LikeComponent {
    public totalLikes : number = 0;

    like() {
        this.totalLikes++;
    }

    dislike() {
        this.totalLikes--;
    }
}
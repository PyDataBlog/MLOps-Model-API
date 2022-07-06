package org.vidogram.messenger.exoplayer.f;

public final class f
{
  public final int a;
  public final int b;
  public final int c;
  public final int d;
  public final int e;
  public final int f;
  public final int g;
  public final long h;

  public f(byte[] paramArrayOfByte, int paramInt)
  {
    paramArrayOfByte = new l(paramArrayOfByte);
    paramArrayOfByte.a(paramInt * 8);
    this.a = paramArrayOfByte.c(16);
    this.b = paramArrayOfByte.c(16);
    this.c = paramArrayOfByte.c(24);
    this.d = paramArrayOfByte.c(24);
    this.e = paramArrayOfByte.c(20);
    this.f = (paramArrayOfByte.c(3) + 1);
    this.g = (paramArrayOfByte.c(5) + 1);
    this.h = paramArrayOfByte.c(36);
  }

  public int a()
  {
    return this.g * this.e;
  }

  public long b()
  {
    return this.h * 1000000L / this.e;
  }
}

/* Location:           G:\programs\dex2jar-2.0\vidogram-dex2jar.jar
 * Qualified Name:     org.vidogram.messenger.exoplayer.f.f
 * JD-Core Version:    0.6.0
 */
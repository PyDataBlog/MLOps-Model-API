package org.vidogram.messenger.g.a;

import java.io.ByteArrayOutputStream;

public class j extends ByteArrayOutputStream
{
  private final b a;

  public j(b paramb, int paramInt)
  {
    this.a = paramb;
    this.buf = this.a.a(Math.max(paramInt, 256));
  }

  private void a(int paramInt)
  {
    if (this.count + paramInt <= this.buf.length)
      return;
    byte[] arrayOfByte = this.a.a((this.count + paramInt) * 2);
    System.arraycopy(this.buf, 0, arrayOfByte, 0, this.count);
    this.a.a(this.buf);
    this.buf = arrayOfByte;
  }

  public void close()
  {
    this.a.a(this.buf);
    this.buf = null;
    super.close();
  }

  public void finalize()
  {
    this.a.a(this.buf);
  }

  public void write(int paramInt)
  {
    monitorenter;
    try
    {
      a(1);
      super.write(paramInt);
      monitorexit;
      return;
    }
    finally
    {
      localObject = finally;
      monitorexit;
    }
    throw localObject;
  }

  public void write(byte[] paramArrayOfByte, int paramInt1, int paramInt2)
  {
    monitorenter;
    try
    {
      a(paramInt2);
      super.write(paramArrayOfByte, paramInt1, paramInt2);
      monitorexit;
      return;
    }
    finally
    {
      paramArrayOfByte = finally;
      monitorexit;
    }
    throw paramArrayOfByte;
  }
}

/* Location:           G:\programs\dex2jar-2.0\vidogram-dex2jar.jar
 * Qualified Name:     org.vidogram.messenger.g.a.j
 * JD-Core Version:    0.6.0
 */
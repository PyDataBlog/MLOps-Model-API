package org.amityregion5.terragame;

public class GameLoop implements Runnable {
	
	private void loop(double delta) {
		
	}

	@Override
	public void run() {
		
			long fpsTimer = System.currentTimeMillis();
			int targetFPS = 30;
			double nsPerUpdate = 1000000000.0 / targetFPS;
			// last update

			double then = System.nanoTime();
			double unprocessed = 0;
			boolean shouldLoop = false;
			
			double lastUpdate = System.nanoTime();

			while (true)
			{
				double now = System.nanoTime();
				unprocessed += (now - then) / nsPerUpdate;
				then = now;
				// update
				while (unprocessed >= 1)
				{
					// update++;
					// update();
					unprocessed--;
					shouldLoop = true;
				}

				if (shouldLoop)
				{
					loop((now - lastUpdate) * targetFPS/1e9);
					lastUpdate = now;
					shouldLoop = false;
				} else
				{
					try
					{
						Thread.sleep(1);
					} catch (InterruptedException e)
					{
						e.printStackTrace();
					}
				}

				if (System.currentTimeMillis() - fpsTimer > 1000)
				{
					// System.out.println("Update=" + update);
					// System.out.println("FPS=" + fps);

					// put code for processing fps data here!!!!

					// update = 0;
					fpsTimer = System.currentTimeMillis();
				}
}
		
	}

}

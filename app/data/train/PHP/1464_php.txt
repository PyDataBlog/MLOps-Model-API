{$Header}

<main role="main" class="main" contenteditable="false">
	<aside id="notifications">
		<!-- Notifications -->
		{if !empty($error)}<div class="notification-error">{$error}</div>{/if}
		{if !empty($success)}<div class="notification-success">{$success}</div>{/if}
	</aside>

	<section class="frame">
		<header>
			<section class="box entry-title">
		         <form method="GET">
		         	<input class="" type="text" name="q" value="{$keyword}" placeholder="Keyword..." />
		         </form>
		    </section>
		</header>

	    <section class="entry-container" id="google-trend">
	    	{$GoogleTrendScript}
	    </section>
		
	</section>
</main>

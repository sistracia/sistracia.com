rm_post_assets:
	find ./wwwroot/post-assets -type f -not -name '.gitignore' -exec rm {} \;

cp_post_assets:
	find ./content/posts -type f -not -name 'index.md' -exec cp {} ./wwwroot/post-assets \;

update_post_assets: rm_post_assets cp_post_assets

dotnet_run:
	dotnet run

dotnet_watch:
	dotnet watch

run: update_post_assets dotnet_run

watch: update_post_assets dotnet_watch

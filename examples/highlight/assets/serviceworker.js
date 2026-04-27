'use strict';

const version = "5";
const asset_cachename = `assets-${version}`;

self.addEventListener('install', ev => {
	console.log('Service worker installed: ', asset_cachename);
	ev.waitUntil(async () => {
		const cache = await caches.open(asset_cachename);
		return cache.addAll([
			'/',
			'/assets/core.css',
			'/assets/colour.css',
			'/assets/manifest.json'
		]);
	});
	// Force this instance of the serviceworker to take over existing clients
	self.skipWaiting();
});

self.addEventListener('activate', ev => {
	console.log('Service worker activate: ', asset_cachename);
	ev.waitUntil(async () => {
		const keys = await caches.keys();
		return Promise.all(
			keys.map(key => {
				if (key !== asset_cachename) return caches.delete(key);
			})
		);
	});
	return self.clients.claim();
});

self.addEventListener('fetch', ev => {
	console.log('Service worker fetch: ', ev.request.url);
});

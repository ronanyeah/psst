const cacheName = 'psst-cache'

self.addEventListener(
  'install',
  e =>
    e.waitUntil(
      caches.open(cacheName)
      .then(
        cache =>
          cache.addAll([
            '/',
            '/index.html',
            '/bundle.js',
            '/manifest.json'
          ])
      )
    )
)

self.addEventListener(
  'activate',
  e => (
    e.waitUntil(
      caches.keys()
      .then(
        cacheKeys =>
          Promise.all(
            cacheKeys
            .filter(
              key =>
                key !== cacheName
            )
            .map(
              key =>
                caches.delete(key)
            )
          )
      )
    ),
    self.clients.claim()
  )
)

self.addEventListener(
  'fetch',
  e =>
    e.respondWith(
      fetch(e.request)
      .catch(
        _err =>
          caches.match(e.request)
      )
    )
)

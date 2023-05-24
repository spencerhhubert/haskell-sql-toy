import adapter from '@sveltejs/adapter-auto';
import { vitePreprocess } from '@sveltejs/kit/vite';

/** @type {import('@sveltejs/kit').Config} */
const config = {
	preprocess: vitePreprocess(),
	kit: {
		adapter: adapter()
	},
    server: {
        proxy: {
            "/api": "http://localhost:4000/",
        }
    }
};

export default config;

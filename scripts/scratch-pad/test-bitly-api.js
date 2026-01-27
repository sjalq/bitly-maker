#!/usr/bin/env node

/**
 * Diagnostic script to test Bitly API directly
 * Usage: node test-bitly-api.js <API_KEY> <LONG_URL>
 *
 * This helps diagnose 400 errors by showing the full response from Bitly
 */

const https = require('https');

const apiKey = process.argv[2];
const longUrl = process.argv[3] || 'https://example.com/test';

if (!apiKey) {
  console.error('Usage: node test-bitly-api.js <API_KEY> [LONG_URL]');
  console.error('');
  console.error('Example:');
  console.error('  node test-bitly-api.js "your-bitly-api-key" "https://example.com/page?param=value"');
  process.exit(1);
}

const requestBody = JSON.stringify({
  long_url: longUrl
});

const options = {
  hostname: 'api-ssl.bitly.com',
  port: 443,
  path: '/v4/shorten',
  method: 'POST',
  headers: {
    'Authorization': `Bearer ${apiKey}`,
    'Content-Type': 'application/json',
    'Content-Length': Buffer.byteLength(requestBody)
  }
};

console.log('=== Bitly API Test ===');
console.log('URL to shorten:', longUrl);
console.log('API Key (first 8 chars):', apiKey.substring(0, 8) + '...');
console.log('');
console.log('Request body:', requestBody);
console.log('');

const req = https.request(options, (res) => {
  let data = '';

  res.on('data', (chunk) => {
    data += chunk;
  });

  res.on('end', () => {
    console.log('=== Response ===');
    console.log('Status Code:', res.statusCode);
    console.log('Status Message:', res.statusMessage);
    console.log('');
    console.log('Headers:', JSON.stringify(res.headers, null, 2));
    console.log('');
    console.log('Body (raw):', data);
    console.log('');

    try {
      const parsed = JSON.parse(data);
      console.log('Body (parsed):', JSON.stringify(parsed, null, 2));
    } catch (e) {
      console.log('Body is not valid JSON');
    }

    if (res.statusCode >= 400) {
      console.log('');
      console.log('=== ERROR ANALYSIS ===');
      try {
        const errorObj = JSON.parse(data);
        if (errorObj.message) {
          console.log('Error Message:', errorObj.message);
        }
        if (errorObj.description) {
          console.log('Error Description:', errorObj.description);
        }
        if (errorObj.errors) {
          console.log('Detailed Errors:', JSON.stringify(errorObj.errors, null, 2));
        }
      } catch (e) {
        console.log('Could not parse error response');
      }
    }
  });
});

req.on('error', (e) => {
  console.error('Request error:', e.message);
});

req.write(requestBody);
req.end();

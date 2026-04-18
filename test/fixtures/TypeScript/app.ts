#!/usr/bin/env nodejs

import axios from 'axios';
import express, { Request, Response } from "express";
import fs from 'fs';
import logger from 'morgan';
import path from 'path';
import Database from './database/database';
/** Routes */
import about from './routes/about/about';
import dashboard from './routes/admin/dashboard/dashboard';
import login from './routes/admin/login/login';
import events from './routes/events/events';
import join from './routes/join/join';
import merch from './routes/merch/merch';
import partners from './routes/partners/partners';
import { logger as LOGGER } from './utils/logger';

const app = express();
const db = new Database();
const tokens = JSON.parse(fs.readFileSync('config/tokens.json', 'utf8'));

/** Logger for HTTP requests. */
app.use(logger('dev'));
/** application/x-www-form-urlencoded. */
app.use(express.urlencoded({ extended: false }));
/** Handles parsing for JSON data. */
app.use(express.json());
/** Set directory for static files. */
app.use(express.static(path.join(__dirname, 'public')));
/** Set the view template path. */
app.set('views', path.join(__dirname, 'views'));
/** Set view engine. */
app.set('view engine', 'pug');

/** Index page */
app.get('/', async (req: Request, res: Response) => {
    LOGGER.info(`[${req.method}] - ${req.path} - Fetching latest tweets`);
    const url = 'https://api.twitter.com/1.1/statuses/user_timeline.json'
        +'?screen_name=enucs&exclude_replies=true&include_rts=false&count=3';
    const bearerToken = 'bearer ' + tokens.twitter;
    let tweets = [];

    try {
        const instance = await axios({ url: url, headers: { 'Authorization': bearerToken }});
        tweets = instance.data.map((tweet: any) => {
            return {
                body: tweet.text,
                created_at: new Date(tweet.created_at)
                    .toLocaleString('en-GB', {
                        day: '2-digit',
                        month: 'short',
                        year: 'numeric',
                        hour: '2-digit',
                        minute: '2-digit'
                    }),
                handle: tweet.user.screen_name
            };
        });
    } catch (e) {
        LOGGER.error('Failed to read data from Twitter API', e);
    }

    let events = await db.getFutureEvents(6);

    res.render('index', {
        events: events.map(event => event.prettifyDates()),
        tweets: tweets || null
    });
});

app.use('/about', about.router);
app.use('/events', events.router);
app.use('/partners', partners.router);
app.use('/merch', merch.router);
app.use('/join', join.router);
app.use('/', login.router);
app.use('/admin/dashboard', dashboard.router);

app.listen(3000, () => {
    LOGGER.info('Listening on port 3000...');
});
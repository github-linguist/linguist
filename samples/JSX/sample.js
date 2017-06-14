'use strict';

const React = require('react')

module.exports = React.createClass({
  render: function() {
    let {feeds, log} = this.props;

    log.info(feeds);
    return <div className="feed-list">
      <h3>News Feed's</h3>
      <ul>
        {feeds.map(function(feed) {
          return <li key={feed.name} className={feed.fetched ? 'loaded' : 'loading'}>
            {feed.data && feed.data.length > 0 ?
              <span>{feed.name} <span className='light'>({feed.data.length})</span></span>
              : 'feed.name' }
          </li>
        })}
      </ul>
    </div>;
  }
});

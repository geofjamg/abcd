/*
 * Copyright (C) 2010 Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.jamgotchian.abcd.core.util;

import java.util.AbstractMap.SimpleImmutableEntry;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NavigableMap;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class RangeMap<K extends Range, V> implements Map<K, V> {

    private final NavigableMap<Integer, Map.Entry<K, V>> map = new TreeMap<>();

    public RangeMap() {
    }

    @Override
    public int size() {
        return map.size();
    }

    @Override
    public boolean isEmpty() {
        return map.isEmpty();
    }

    @Override
    public boolean containsKey(Object key) {
        if (key == null) {
            throw new NullPointerException();
        }
        K k = (K) key;
        Map.Entry<K, V> entry = map.get(k.getFirst());
        if (entry != null) {
            return entry.getKey().equals(key);
        } else {
            return false;
        }
    }

    @Override
    public boolean containsValue(Object value) {
        for (Map.Entry<K, V> entry : map.values()) {
            if (entry.getValue().equals(value)) {
                return true;
            }
        }
        return false;
    }

    @Override
    public V get(Object key) {
        if (key == null) {
            throw new NullPointerException();
        }
        K k = (K) key;
        Map.Entry<K, V> entry = map.get(k.getFirst());
        if (entry != null && entry.getKey().equals(key)) {
            return entry.getValue();
        } else {
            return null;
        }
    }

    @Override
    public V put(K key, V value) {
        if (key == null) {
            throw new NullPointerException();
        }
        K k = key;
        V previousValue = findBeginningAt(k.getFirst());
        if (previousValue != null) {
            map.put(key.getFirst(), new SimpleImmutableEntry<>(key, value));
        } else {
            if (findContaining(k.getFirst()) != null) {
                throw new IllegalArgumentException("Range overlapping");
            } else {
                map.put(key.getFirst(), new SimpleImmutableEntry<>(key, value));
            }
        }
        return previousValue;
    }

    @Override
    public V remove(Object key) {
        if (key == null) {
            throw new NullPointerException();
        }
        K k = (K) key;
        V value = get(k);
        if (value != null) {
            map.remove(k.getFirst());
            return value;
        } else {
            return null;
        }
    }

    @Override
    public void putAll(Map<? extends K, ? extends V> m) {
        for (Map.Entry<? extends K, ? extends V> entry : m.entrySet()) {
            put(entry.getKey(), entry.getValue());
        }
    }

    @Override
    public void clear() {
        map.clear();
    }

    @Override
    public Set<K> keySet() {
        Set<K> keys = new TreeSet<>();
        for (Map.Entry<K, V> entry : map.values()) {
            keys.add(entry.getKey());
        }
        return keys;
    }

    @Override
    public Collection<V> values() {
        List<V> values = new ArrayList<>(map.size());
        for (Map.Entry<K, V> entry : map.values()) {
            values.add(entry.getValue());
        }
        return values;
    }

    @Override
    public Set<Entry<K, V>> entrySet() {
        return new LinkedHashSet<>(map.values());
    }

    public V findBeginningAt(int index) {
        Map.Entry<K, V> entry = map.get(index);
        if (entry == null) {
            return null;
        } else {
            return entry.getValue();
        }
    }

    public V findContaining(int index) {
        Map.Entry<Integer, Map.Entry<K, V>> entry = map.floorEntry(index);
        if (entry == null) {
            return null;
        } else {
            K key = entry.getValue().getKey();
            if (key.contains(index)) {
                return entry.getValue().getValue();
            } else {
                return null;
            }
        }
    }

    public Collection<V> values(Range range) {
        List<V> values = new ArrayList<>();
        for (Map.Entry<K, V> entry : map.values()) {
            if (range.contains(entry.getKey())) {
                values.add(entry.getValue());
            }
        }
        return values;
    }

    @Override
    public String toString() {
        Iterator<Entry<K,V>> i = map.values().iterator();
        if (!i.hasNext()) {
            return "{}";
        }
        StringBuilder sb = new StringBuilder();
        sb.append('{');
        for (;;) {
            Entry<K,V> e = i.next();
            K key = e.getKey();
            V value = e.getValue();
            sb.append(key)
              .append('=')
              .append(value == this ? "(this Map)" : value);
            if (!i.hasNext()) {
                return sb.append('}').toString();
            }
            sb.append(", ");
        }
    }

}
